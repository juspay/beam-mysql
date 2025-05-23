{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy         #-}

module Database.Beam.MySQL.Extra where

import           Control.Exception.Safe (bracket, throw)
import           Control.Monad (when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (MonadReader (ask), asks)
import           Data.Foldable (traverse_)
import           Data.Functor.Identity (Identity)
import           Data.Kind (Type)
import           Data.Text (Text, pack)
import           Data.Time (LocalTime, localTimeToUTC, utc, addLocalTime, utctDayTime)
import           Data.Text.Lazy (fromStrict, toStrict)
import           Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import           Data.Vector (Vector, find, foldl1', head, length, mapMaybe,
                              null, unfoldrM, zip, (!?))
import           Database.Beam.Backend.SQL.Row (FromBackendRow)
import           Database.Beam.MySQL.Connection (MySQL, MySQLM (MySQLM),
                                                 MySQLMEnv (..),
                                                 MySQLStatementError (..),
                                                 acquireStream, drainStream)
import           Database.Beam.MySQL.Syntax.Insert (MySQLInsert,
                                                    MySQLInsertValuesSyntax (..))
import           Database.Beam.MySQL.Syntax.Misc (MySQLFieldNameSyntax (..))
import           Database.Beam.MySQL.Syntax.Render (RenderError (..),
                                                    RenderErrorType (..),
                                                    renderDelete, renderInsert,
                                                    renderSelect, renderUpdate)
import           Database.Beam.MySQL.Syntax.Select (BinOp (LAnd), CompOp (CEq),
                                                    MySQLExpressionSyntax (..),
                                                    MySQLFromSyntax (..),
                                                    MySQLProjectionSyntax (..),
                                                    MySQLSelect (SelectStmt),
                                                    MySQLSelectTableSyntax (..),
                                                    MySQLTableNameSyntax,
                                                    MySQLTableSourceSyntax (TableNamed),
                                                    Projection (..),
                                                    TableHeader (Anonymous),
                                                    TableRowExpression (..))
import           Database.Beam.MySQL.Syntax.Value (MySQLValueSyntax (..))
import           Database.Beam.Query (SqlDelete (..), SqlInsert (..),
                                      SqlSelect (..), SqlUpdate (..),
                                      runSelectReturningOne)
import           Database.MySQL.Base (ColumnDef, MySQLConn,
                                      MySQLValue (MySQLText), Query (Query),
                                      execute_, okAffectedRows)
import           Prelude hiding (head, length, null, read, zip)
import           System.IO.Streams (InputStream, read)

-- | Attempts to render the given 'SqlInsert' into the equivalent SQL text. Will
-- produce 'Nothing' if the 'SqlInsert' corresponds to inserting no rows, or is
-- not validly formed.
--
-- = Note
--
-- Unless you are constructing MySQL ASTs by hand (that is, not using the
-- Beam-provided type class methods), invalid SQL should never arise.
--
-- @since 1.2.2.0
dumpInsertSQL :: forall (table :: (Type -> Type) -> Type) .
  SqlInsert MySQL table -> Maybe Text
dumpInsertSQL = \case
  SqlInsertNoRows -> Nothing
  SqlInsert _ ins -> case renderInsert ins of
    Left _                 -> Nothing
    Right (Query query, _) -> Just . toStrict . decodeUtf8 $ query

-- | Attempts to render the given 'SqlSelect' into the equivalent SQL text. Will
-- produce 'Nothing' if the 'SqlSelect' is not validly formed.
--
-- = Note
--
-- Unless you are constructing MySQL ASTs by hand (that is, not using the
-- Beam-provided type class methods), invalid SQL should never arise.
--
-- @since 1.2.2.0
dumpSelectSQL :: forall (a :: Type) .
  SqlSelect MySQL a -> Maybe Text
dumpSelectSQL (SqlSelect sel) = case renderSelect sel of
  Left _                 -> Nothing
  Right (Query query, _) -> Just . toStrict . decodeUtf8 $ query

-- | Attempts to render the given 'SqlUpdate' into the equivalent SQL text. Will
-- produce 'Nothing' if the 'SqlUpdate' corresponds to the identity update (that
-- is, updates no rows), or if it is not validly formed.
--
-- = Note
--
-- Unless you are constructing MySQL ASTs by hand (that is, not using the
-- Beam-provided type class methods), invalid SQL should never arise.
--
-- @since 1.2.2.0
dumpUpdateSQL :: forall (table :: (Type -> Type) -> Type) .
  SqlUpdate MySQL table -> Maybe Text
dumpUpdateSQL = \case
  SqlIdentityUpdate -> Nothing
  SqlUpdate _ upd -> case renderUpdate upd of
    Left _                 -> Nothing
    Right (Query query, _) -> Just . toStrict . decodeUtf8 $ query

-- | Attempts to render the given 'SqlDelete' into the equivalent SQL text. Will
-- produce 'Nothing' if the 'SqlDelete' is not validly formed.
--
-- = Note
--
-- Unless you are constructing MySQL ASTs by hand (that is, not using the
-- Beam-provided type class methods), invalid SQL should never arise.
--
-- @since 1.2.2.0
dumpDeleteSQL :: forall (table :: (Type -> Type) -> Type) .
  SqlDelete MySQL table -> Maybe Text
dumpDeleteSQL (SqlDelete _ del) = case renderDelete del of
  Left _                 -> Nothing
  Right (Query query, _) -> Just . toStrict . decodeUtf8 $ query

-- | Attempts to execute an insert, and returns the inserted row if successful.
--
-- = Caveats
--
-- This operation should /not/ be used outside of a transaction, as it is not
-- atomic. We cannot check for this, and if run outside of a transaction, the
-- results of this operation are unspecified - it might fail, or produce the
-- wrong result, silently and without warning.
--
-- The insert should be constructed in such a way that the values being inserted
-- are produced using only literals, or expressions which are /pure/ (meaning
-- \'replayable any number of times to the same results\'). If they are not,
-- this will throw an 'ImpureExpression', which you should be prepared to catch.
-- As we check this prior to executing anything, if this exception is thrown,
-- the database will /not/ be modified.
--
-- The given insert statement must insert at most a single row. If it inserts
-- multiple rows, an 'OperationNotSupported' will be thrown, which you should be
-- prepared to catch. Since this check is done before executing anything, if
-- this exception is thrown, the database will /not/ be modified.
--
-- The table being targeted by the given insert statement must have a primary
-- key. If it does not, an 'OperationNotSupported' will be thrown. Since this
-- check is done before executing anything, if this exception is thrown, the
-- database will /not/ be modified.
--
-- = Purity
--
-- Literal expressions are always pure. We consider @CURRENT_TIMESTAMP@ to also
-- be pure, as we assume that 'runInsertRowReturning' is running inside a
-- transaction. We also assume that @DEFAULT@ is pure, as we do not support
-- MySQL 8 or later, where it first became (possibly) impure.
--
-- Any \'compound\' expressions are considered pure if their components all are.
-- The exception is anything involving sub-@SELECT@s; this analysis is a little
-- crude, but we don't currently require this functionality.
--
-- @since 1.2.2.0
runInsertRowReturning :: forall (table :: (Type -> Type) -> Type) .
  (FromBackendRow MySQL (table Identity)) =>
  SqlInsert MySQL table -> MySQLM (Maybe (table Identity))
runInsertRowReturning stmt = case stmt of
  SqlInsertNoRows -> pure Nothing
  SqlInsert _ ins -> case renderInsert ins of
    Left (RenderError typ ast) -> case typ of
      UnsupportedOperation op -> unsupported op ast ins
    Right (query, _) -> case ins.insertValues of
      InsertFromSQL sel -> insertIntoSelect sel ins
      InsertSQLExpressions rows -> case length rows of
        0 -> pure Nothing -- should be impossible
        1 -> insertRowReturning ins (head rows) query
        _ -> multipleRowInserts rows ins

-- Helpers

-- Error reporting

unsupported :: Text -> Text -> MySQLInsert -> MySQLM a
unsupported op ast = throw . OperationNotSupported op ast . pack . show

insertIntoSelect :: MySQLSelect -> MySQLInsert -> MySQLM a
insertIntoSelect sel =
  throw .
    OperationNotSupported "Insert-into-select with returning" (pack . show $ sel) .
    pack .
    show

multipleRowInserts :: Vector TableRowExpression -> MySQLInsert -> MySQLM a
multipleRowInserts rows =
  throw .
    OperationNotSupported "Multiple row inserts with returning" (pack . show $ rows) .
    pack .
    show

impureExpr :: MySQLExpressionSyntax -> MySQLInsert -> MySQLM a
impureExpr e = throw . ImpureExpression (pack . show $ e) . pack . show

noPrimaryKey :: MySQLInsert -> MySQLM ()
noPrimaryKey ins =
  throw .
  OperationNotSupported "Insert row returning without primary key" ins.tableName.name .
  pack . show $ ins

-- Core logic

insertRowReturning :: forall (table :: (Type -> Type) -> Type) .
  (FromBackendRow MySQL (table Identity)) =>
  MySQLInsert ->
  TableRowExpression ->
  Query ->
  MySQLM (Maybe (table Identity))
insertRowReturning ins (TableRowExpression v) query@(Query inner) = do
  -- Ensure that all of our inserted values come from pure expressions
  traverse_ (analyzeExpr ins) v
  -- Collect a vector of column names and corresponding values. This _has_ to
  -- retain the order exactly as specified by the insert, or we risk breaking
  -- subsequent queries.
  let fieldVals = zip ins.columns v
  env <- MySQLM ask
  conn <- case env of
    DebugEnv dbg conn -> do
      let textual = toStrict . decodeUtf8 $ inner
      liftIO (dbg textual) >> pure conn
    ReleaseEnv conn -> pure conn
  -- Get the names of all primary key columns in the table.
  pkColNames <- getPkCols conn ins.tableName.name ins.tableName.schema
  -- If we don't find anything, abort.
  when (null pkColNames) (noPrimaryKey ins)
  -- Determine if we have an auto-increment column, and if so, what it is
  mAIColumn <- getAutoIncColumn conn ins.tableName.name ins.tableName.schema
  -- Run the insert
  res <- liftIO . execute_ conn $ query
  case okAffectedRows res of
    -- This means our insert missed for some reason, so we have nothing to give
    -- back.
    0 -> pure Nothing
    -- The insert hit, so we need to find out what changed.
    _ -> do
      -- Build a SELECT on the basis of all our gleaned information
      let sel =
            SqlSelect .
              buildPostSelect ins.tableName fieldVals pkColNames $ mAIColumn
      -- Execute to get our changes
      runSelectReturningOne sel

-- Marks the expression as pure (meaning, repeatable any number of times) and
-- impure (meaning side effecting).
data Purity = Pure | Impure
  deriving stock (Eq, Show)

instance Semigroup Purity where
  Impure <> _ = Impure
  Pure <> x   = x

instance Monoid Purity where
  mempty = Pure

analyzeExpr :: MySQLInsert -> MySQLExpressionSyntax -> MySQLM ()
analyzeExpr ins e = case go e of
  Pure   -> pure ()
  Impure -> impureExpr e ins
  where
    go :: MySQLExpressionSyntax -> Purity
    go e' = case e' of
      -- Literals are always pure
      Value _             -> Pure
      -- A row is pure if all its components are
      Row rowEs           -> foldMap go rowEs
      -- A coalesce operation is pure if all its components are
      Coalesce coalesceEs -> foldMap go coalesceEs
      -- A case expression is pure if all its components are. Realistically,
      -- that makes it a constant, since we either always meet one condition, or
      -- always meet none.
      Case{}              ->
        foldMap (\cb -> go cb.condition <> go cb.action) e'.cases <> go e'.defaultCase
      -- A field reference is always impure.
      Field{} -> Impure
      -- A binary operation is pure if its operands are.
      BinaryOperation{} -> go e'.lOperand <> go e'.rOperand
      -- A comparison operation is pure if its operands are.
      ComparisonOperation{} -> go e'.lOperand <> go e'.rOperand
      -- A prefix operation is pure if its operand is.
      PrefixOperation{} -> go e'.operand
      -- A postfix operation is pure if its operand is.
      PostfixOperation{} -> go e'.operand
      -- A NULLIF is pure if both its branches are.
      NullIf{} -> go e'.expr <> go e'.ifNull
      -- A POSITION is pure if the needle and haystack are both pure.
      Position{} -> go e'.needle <> go e'.haystack
      -- A CAST is pure if the expression being cast is.
      Cast{} -> go e'.expr
      -- Field extractions from pure expressions are pure.
      Extract{} -> go e'.expr
      JSONExtract{} -> go e'.expr <> go e'.key
      -- CURRENT_TIMESTAMP is pure within a transaction, which we assume we're
      -- in, as runInsertRowReturning is unsafe outside one.
      CurrentTimestamp -> Pure
      -- DEFAULT cannot be assumed pure in general after MySQL 8.0.13. Given
      -- that we're not on this version, we're safe.
      Default -> Pure
      -- IN expressions are pure if all their components are.
      In{} -> foldMap go e'.exprs <> go e'.expr
      -- BETWEEN is pure if all its components are.
      Between{} -> go e'.expr <> go e'.lo <> go e'.hi
      -- Anything involving sub-SELECTs is assumed impure. This is a bit crude,
      -- but realistic analysis of these would be too hard for our goals.
      Exists{} -> Impure
      Unique{} -> Impure
      Subquery{} -> Impure
      -- Any aggregation is pure, provided the expression being aggregated on
      -- is.
      CountAll -> Pure
      Aggregation{} -> go e'.expr
      -- CONCAT is pure if all expressions being concatenated are.
      Concat concatEs -> foldMap go concatEs
      -- This should never come up, since beam never constructs this
      LastInsertId -> Impure

getConnection :: MySQLM MySQLConn
getConnection = MySQLM (asks go)
  where
    go :: MySQLMEnv -> MySQLConn
    go = \case
      DebugEnv _ c -> c
      ReleaseEnv c -> c

getPkCols :: MySQLConn -> Text -> Maybe Text -> MySQLM (Vector Text)
getPkCols conn nam mbSchemaName = do
  let query = Query $
        "SELECT key_column_usage.column_name " <>
        "FROM information_schema.key_column_usage " <>
        "WHERE table_schema = " <>
        maybe "schema()" (\sn -> encodeUtf8 . fromStrict $ "'" <> sn <> "'") mbSchemaName <>
        " AND constraint_name = 'PRIMARY' " <>
        "AND table_name = '" <>
        (encodeUtf8 . fromStrict $ nam) <>
        "';"
  bracket (acquireStream conn query)
          drainStream
          (liftIO . unfoldrM go)
  where
    go :: (Vector ColumnDef, InputStream (Vector MySQLValue)) ->
      IO (Maybe (Text, (Vector ColumnDef, InputStream (Vector MySQLValue))))
    go (env, stream) = do
      res <- read stream
      pure $ (, (env, stream)) <$> (res >>= (!? 0) >>= extractText)

getAutoIncColumn :: MySQLConn -> Text -> Maybe Text -> MySQLM (Maybe Text)
getAutoIncColumn conn nam mbSchemaName = do
  let query = Query $
        "SELECT column_name " <>
        "FROM information_schema.columns " <>
        "WHERE table_schema = " <>
        maybe "schema()" (\sn -> encodeUtf8 . fromStrict $ "'" <> sn <> "'") mbSchemaName <>
        " AND table_name = '" <>
        (encodeUtf8 . fromStrict $ nam) <>
        "' AND extra LIKE 'auto_increment' " <>
        "LIMIT 1;"
  bracket (acquireStream conn query)
          drainStream
          (liftIO . go)
  where
    go :: (Vector ColumnDef, InputStream (Vector MySQLValue)) -> IO (Maybe Text)
    go (_, stream) = do
      res <- read stream
      pure $ res >>= (!? 0) >>= extractText

extractText :: MySQLValue -> Maybe Text
extractText = \case
  MySQLText t -> pure t
  _           -> Nothing

buildPostSelect ::
  MySQLTableNameSyntax ->
  Vector (Text, MySQLExpressionSyntax) ->
  Vector Text ->
  Maybe Text ->
  MySQLSelect
buildPostSelect nam fieldVals pkCols mAICol =
  -- Order doesn't matter, LIMIT 1, no offset
  SelectStmt selTable mempty (Just 1) Nothing
  where
    selTable :: MySQLSelectTableSyntax
    selTable =
      SelectTableStatement
        Nothing
        Nothing -- no quantifier
        fieldsOf
        (Just . FromTable ourName $ Anonymous)
        (Just onPKVals)
        Nothing -- no GROUP BY
        Nothing -- no HAVING
    fieldsOf :: MySQLProjectionSyntax
    fieldsOf = ProjectExpressions . fmap (projectField . fst) $ fieldVals
    projectField :: Text -> Projection
    projectField t = Projection (Field . UnqualifiedField $ t) Nothing
    ourName :: MySQLTableSourceSyntax
    ourName = TableNamed nam
    onPKVals :: MySQLExpressionSyntax
    onPKVals =
      foldl1' (BinaryOperation LAnd) .
        mapMaybe (uncurry fieldEqExpr) $ fieldVals
    fieldEqExpr :: Text -> MySQLExpressionSyntax -> Maybe MySQLExpressionSyntax
    fieldEqExpr f e = do
      -- if it's not a primary key column, we can ignore it
      _ <- find (f ==) pkCols
      rOp <- case (f ==) <$> mAICol of
                -- If the autoincrementing column is in our primary key, we have
                -- more analysis to do.
                Just True -> pure $ case e of
                  -- If this is either zero, or NULL, the insert will trigger
                  -- autoincrement behaviour as per MySQL documentation. If it's
                  -- anything else, we have to paste a literal.
                  --
                  -- Floating-point does not get included in this analysis,
                  -- since the concept of both 'exact zero' and 'automatic
                  -- increment' makes no sense.
                  Value v -> case v of
                    VInt8 i       -> case i of
                      0 -> LastInsertId
                      _ -> e
                    VInt16 i      -> case i of
                      0 -> LastInsertId
                      _ -> e
                    VInt32 i      -> case i of
                      0 -> LastInsertId
                      _ -> e
                    VInt64 i      -> case i of
                      0 -> LastInsertId
                      _ -> e
                    VWord8 w      -> case w of
                      0 -> LastInsertId
                      _ -> e
                    VWord16 w     -> case w of
                      0 -> LastInsertId
                      _ -> e
                    VWord32 w     -> case w of
                      0 -> LastInsertId
                      _ -> e
                    VWord64 w     -> case w of
                      0 -> LastInsertId
                      _ -> e
                    VScientific s -> if s == 0 then LastInsertId else e
                    VNull         -> LastInsertId
                    VNothing      -> LastInsertId
                    _             -> e
                  -- If this is anything else, the only sensible possibility is
                  -- DEFAULT (our generator and beam together guarantee this).
                  -- If that's the case, we can just replace it with
                  -- last_insert_id().
                  _       -> LastInsertId
                -- If we don't have an autoincrementing column in our primary
                -- key, or this isn't it, just paste the value as-was.
                _         -> pure $ case e of
                  Value (VLocalTime v) -> Value (VLocalTime $ stripMilliSeconds v)
                  _ -> e
      let lOp = Field . UnqualifiedField $ f
      pure . ComparisonOperation CEq Nothing lOp $ rOp
    {-- Handled this to query entry with datetime datatypes primary Id.
        While inserting datetime format, mysql rounds off milliseconds (>= 500000000000) to next second.
        Added logic to do the same for fetching as well.
    --}
    stripMilliSeconds :: LocalTime -> LocalTime
    stripMilliSeconds curr =
      let utcCurrentTime = localTimeToUTC utc curr
          actual = (fromEnum $ utctDayTime utcCurrentTime) `rem` 1000000000000
          nominalDiffTime = 
            if actual >= 500000000000
              then 1000000000000 - actual
              else 0 - actual
      in addLocalTime (toEnum nominalDiffTime) curr

    {-
    fieldEqExpr f e = do
      -- if it's not a primary key column, we ignore it
      _ <- find (f ==) pkCols
      rOp <- case (f ==) <$> mAICol of
              -- if the autoincrementing column is in our primary key, replace
              -- its DEFAULT with last_insert_id()
              Just True -> pure LastInsertId
              -- if we don't have an autoincrementing column in our primary key,
              -- or this isn't it, just paste the value as-was
              _         -> pure e
      let lOp = Field . UnqualifiedField $ f
      pure . ComparisonOperation CEq Nothing lOp $ rOp -}
