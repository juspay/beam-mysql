{-# LANGUAGE KindSignatures #-}

module Database.Beam.MySQL.Syntax.Render where

import           Control.Monad.Except (MonadError (throwError))
import           Control.Monad.Writer.Strict (MonadWriter (tell), WriterT,
                                              runWriterT)
import           Data.Aeson (encode)
import           Data.Bifunctor (first)
import qualified Data.Binary.Builder as Bin
import           Data.Bool (bool)
import           Data.ByteString.Builder.Scientific (FPFormat (Fixed),
                                                     formatScientificBuilder)
import           Data.ByteString.Lazy (toStrict)
import           Data.Foldable (fold)
import           Data.HashSet (HashSet, singleton)
import           Data.Kind (Type)
import           Data.String (IsString)
import           Data.Text (Text, pack)
import           Data.Text.Encoding (decodeLatin1)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Data.Vector (Vector, length, toList)
import           Database.Beam.MySQL.Syntax.DataType (MySQLDataTypeSyntax (..),
                                                      MySQLPrecision (..))
import           Database.Beam.MySQL.Syntax.Delete (MySQLDelete)
import           Database.Beam.MySQL.Syntax.Insert (MySQLInsert,
                                                    MySQLInsertValuesSyntax (..))
import           Database.Beam.MySQL.Syntax.Misc (MySQLAggregationSetQuantifierSyntax (..),
                                                  MySQLExtractFieldSyntax (..),
                                                  MySQLFieldNameSyntax (..),
                                                  MySQLQuantifierSyntax (..))
import           Database.Beam.MySQL.Syntax.Select (AggOp (..), BinOp (..),
                                                    CaseBranch, CompOp (..),
                                                    MySQLExpressionSyntax (..),
                                                    MySQLFromSyntax (..),
                                                    MySQLGroupingSyntax (..),
                                                    MySQLOrderingSyntax (..),
                                                    MySQLProjectionSyntax (..),
                                                    MySQLSelect,
                                                    MySQLSelectTableSyntax (..),
                                                    MySQLTableNameSyntax,
                                                    MySQLTableSourceSyntax (..),
                                                    PostOp (..), PrefOp (..),
                                                    Projection,
                                                    TableHeader (..),
                                                    TableRowExpression (..))
import           Database.Beam.MySQL.Syntax.Update (FieldUpdate, MySQLUpdate)
import           Database.Beam.MySQL.Syntax.Value (MySQLValueSyntax (..))
import           Database.MySQL.Base (Query (Query))
import           Database.MySQL.Protocol.Escape (escapeBytes, escapeText)
import           Mason.Builder (BuilderFor, LazyByteStringBackend, byteString,
                                doubleDec, floatDec, int16Dec, int32Dec,
                                int64Dec, int8Dec, integerDec, intersperse,
                                lazyByteString, textUtf8, toLazyByteString,
                                word16Dec, word32Dec, word64Dec, word8Dec,
                                wordDec)
import           Prelude hiding (length)

newtype RenderErrorType = UnsupportedOperation Text
  deriving stock (Show)
  deriving newtype (Eq)

data RenderError = RenderError {
  errorType   :: !RenderErrorType,
  astFragment :: {-# UNPACK #-} !Text
  }
  deriving stock (Eq, Show)

renderSelect :: MySQLSelect -> Either RenderError (Query, HashSet Text)
renderSelect = runRender . fmap (<> ";") . renderSelect'

renderInsert :: MySQLInsert -> Either RenderError (Query, HashSet Text)
renderInsert = runRender . fmap (<> ";") . renderInsert'

renderUpdate :: MySQLUpdate -> Either RenderError (Query, HashSet Text)
renderUpdate = runRender . fmap (<> ";") . renderUpdate'

renderDelete :: MySQLDelete -> Either RenderError (Query, HashSet Text)
renderDelete = runRender . fmap (<> ";") . renderDelete'

-- Helpers

-- ugh
type Builder = BuilderFor LazyByteStringBackend

newtype RenderM (a :: Type) =
  RenderM (WriterT (HashSet Text) (Either RenderError) a)
  deriving newtype (Functor,
                    Applicative,
                    Monad,
                    MonadWriter (HashSet Text),
                    MonadError RenderError)

runRender ::
  RenderM Builder ->
  Either RenderError (Query, HashSet Text)
runRender (RenderM comp) = first (Query . toLazyByteString) <$> runWriterT comp

renderSelect' :: MySQLSelect -> RenderM Builder
renderSelect' sel = do
  table' <- renderSelectTable sel.table
  orderings' <- traverse renderOrdering sel.orderings
  let limit' = fmap integerDec sel.limit
  let offset' = fmap integerDec sel.offset
  pure $
    table' <>
    go orderings' <>
    go2 limit' offset'
  where
    go :: Vector Builder -> Builder
    go v = if Data.Vector.length v == 0
      then mempty
      else " ORDER BY " <> (intersperse ", " . toList $ v)
    go2 :: Maybe Builder -> Maybe Builder -> Builder
    go2 limit' offset' = case limit' of
      Nothing -> mempty
      Just lim -> case offset' of
        Nothing  -> " LIMIT " <> lim
        Just off -> " LIMIT " <> off <> ", " <> lim

renderSelectTable :: MySQLSelectTableSyntax -> RenderM Builder
renderSelectTable sts = case sts of
  UnionTablesAll{}      -> do
    l' <- renderSelectTable sts.lTable
    r' <- renderSelectTable sts.rTable
    pure $ l' <> " UNION ALL " <> r'
  UnionTablesDistinct{} -> do
    l' <- renderSelectTable sts.lTable
    r' <- renderSelectTable sts.rTable
    pure $ l' <> " UNION DISTINCT " <> r'
  IntersectTables{}     -> invalidOp "INTERSECT" sts
  ExceptTable{}         -> invalidOp "EXCEPT" sts
  _                     -> do
    q <- traverse renderSetQuantifier sts.quantifier
    proj <- renderProjection sts.projection
    from' <- traverse renderFrom sts.from
    wher' <- traverse renderExpr sts.wher
    grouping' <- traverse renderGrouping sts.grouping
    having' <- traverse renderExpr sts.having
    pure $
      "SELECT " <>
      foldMap (" " <>) q <>
      proj <>
      foldMap (" FROM " <>) from' <>
      foldMap (" WHERE " <>) wher' <>
      foldMap (" GROUP BY " <>) grouping' <>
      foldMap (" HAVING " <>) having'

renderSetQuantifier :: MySQLAggregationSetQuantifierSyntax -> RenderM Builder
renderSetQuantifier = pure . \case
  SetDistinct -> "DISTINCT"
  SetAll      -> "ALL"

renderProjection :: MySQLProjectionSyntax -> RenderM Builder
renderProjection (ProjectExpressions es) = do
  projs <- traverse go es
  pure . intersperse ", " . toList $ projs
  where
    go :: Projection -> RenderM Builder
    go p = do
      e' <- renderExpr p.expr
      pure $ e' <> foldMap ((" AS " <>) . backtickWrap . textUtf8) p.label

renderFrom :: MySQLFromSyntax -> RenderM Builder
renderFrom fs = case fs of
  FromTable{} -> do
    src <- renderTableSource fs.tableSource
    hdr <- renderTableHeader fs.tableHeader
    pure $ src <> hdr
  InnerJoin{} -> do
    (l, r, c) <- renderJoinParts fs.leftArg fs.rightArg fs.condition
    pure $ l <> " JOIN " <> r <> fold c
  LeftJoin{}  -> do
    (l, r, c) <- renderJoinParts fs.leftArg fs.rightArg fs.condition
    pure $ l <> " LEFT JOIN " <> r <> fold c
  RightJoin{} -> do
    (l, r, c) <- renderJoinParts fs.leftArg fs.rightArg fs.condition
    pure $ l <> " RIGHT JOIN " <> r <> fold c

renderJoinParts ::
  MySQLFromSyntax ->
  MySQLFromSyntax ->
  Maybe MySQLExpressionSyntax ->
  RenderM (Builder, Builder, Maybe Builder)
renderJoinParts l r c =
  (,,) <$> renderFrom l <*> renderFrom r <*> traverse renderExpr c

renderTableSource :: MySQLTableSourceSyntax -> RenderM Builder
renderTableSource = \case
  TableNamed nam -> renderTableName nam
  TableFromSubSelect sel -> bracketWrap <$> renderSelect' sel
  TableFromValues rs ->
    bracketWrap . intersperse " UNION " . toList . fmap ("SELECT " <>) <$>
      traverse renderTableRow rs

renderTableName :: MySQLTableNameSyntax -> RenderM Builder
renderTableName tns = do
  tell . singleton $ tns.name
  pure $ foldMap textUtf8 tns.schema <> (backtickWrap . textUtf8 $ tns.name)

renderTableRow :: TableRowExpression -> RenderM Builder
renderTableRow (TableRowExpression v) = do
  v' <- traverse renderExpr v
  pure . intersperse ", " . toList $ v'

renderTableHeader :: TableHeader -> RenderM Builder
renderTableHeader = pure . \case
  Anonymous -> mempty
  TableNameOnly nam -> " AS " <> (backtickWrap . textUtf8 $ nam)
  TableAndColumns nam cols ->
    " AS " <>
    (backtickWrap . textUtf8 $ nam) <>
    (bracketWrap . intersperse ", " . toList . fmap (backtickWrap . textUtf8) $ cols)

renderExpr :: MySQLExpressionSyntax -> RenderM Builder
renderExpr es = case es of
  Value v -> renderValue v
  Row rowEs -> do
    es' <- traverse renderExpr rowEs
    pure . bracketWrap . intersperse ", " . toList $ es'
  Coalesce coalesceEs -> do
    exprs' <- traverse renderExpr coalesceEs
    pure . ("COALESCE " <>) . bracketWrap . intersperse ", " . toList $ exprs'
  Case{} -> do
    cases' <- traverse renderCaseBranch es.cases
    def <- renderExpr es.defaultCase
    pure $
      "CASE " <>
      (intersperse " " . toList $ cases') <>
      " ELSE " <>
      def <>
      " END"
  Field nam -> renderFieldName nam
  BinaryOperation{} -> do
    op' <- renderBinOp es.binOp
    l' <- renderExpr es.lOperand
    r' <- renderExpr es.rOperand
    pure $
      bracketWrap l' <>
      " " <>
      op' <>
      " " <>
      bracketWrap r'
  ComparisonOperation{} -> do
    op' <- renderCompOp es.compOp
    q' <- traverse renderQuantifier es.quantifier
    l' <- renderExpr es.lOperand
    r' <- renderExpr es.rOperand
    pure $
      bracketWrap l' <>
      " " <>
      op' <>
      fold q' <>
      " " <>
      bracketWrap r'
  PrefixOperation{} -> do
    op' <- renderPrefOp es.prefOp
    e' <- renderExpr es.operand
    pure $ op' <> bracketWrap e'
  PostfixOperation{} -> do
    op' <- renderPostOp es.postOp
    e' <- renderExpr es.operand
    pure $ bracketWrap e' <> op'
  NullIf{} -> do
    e' <- renderExpr es.expr
    ifNull' <- renderExpr es.ifNull
    pure $
      "NULLIF " <>
      bracketWrap (e' <> ", " <> ifNull')
  Position{} -> do
    needle' <- renderExpr es.needle
    haystack' <- renderExpr es.haystack
    pure $
      "POSITION" <>
      bracketWrap (bracketWrap needle' <>
                    " IN " <>
                    bracketWrap haystack')
  Cast{} -> do
    target' <- renderCastTarget es.target
    e' <- renderExpr es.expr
    pure $
      "CAST" <>
      bracketWrap (bracketWrap e' <>
                    " AS " <>
                    target')
  Extract{} -> do
    field' <- renderExtractField es.field
    e' <- renderExpr es.expr
    pure $
      "EXTRACT" <>
      bracketWrap (field' <> " FROM " <> e')
  CurrentTimestamp -> pure "CURRENT_TIMESTAMP"
  Default -> pure "DEFAULT"
  In{} -> do
    e' <- renderExpr es.expr
    es' <- traverse renderExpr es.exprs
    pure $
      bracketWrap e' <>
      " IN " <>
      (bracketWrap . intersperse ", " . toList $ es')
  Between{} -> do
    arg' <- renderExpr es.expr
    lo' <- renderExpr es.lo
    hi' <- renderExpr es.hi
    pure $
      bracketWrap arg' <>
      " BETWEEN " <>
      bracketWrap lo' <>
      " AND " <>
      bracketWrap hi'
  Exists sel -> do
    sel' <- renderSelect' sel
    pure $
      "EXISTS" <>
      bracketWrap sel'
  Unique sel -> do
    sel' <- renderSelect' sel
    pure $
      "UNIQUE" <>
      bracketWrap sel'
  Subquery sel -> do
    sel' <- renderSelect' sel
    pure . bracketWrap $ sel'
  CountAll -> pure "COUNT(*)"
  Aggregation{} -> do
    op' <- renderAggOp es.aggregationOp
    quant' <- traverse renderSetQuantifier es.setQuantifier
    e' <- renderExpr es.expr
    pure $
      op' <>
      bracketWrap (foldMap (<> " ") quant' <> e')
  Concat concatEs -> do
    es' <- traverse renderExpr concatEs
    if Data.Vector.length concatEs == 0
    then pure mempty
    else pure $
      "CONCAT" <>
      bracketWrap (intersperse ", " . toList $ es')
  LastInsertId -> pure "last_insert_id()"

renderAggOp :: AggOp -> RenderM Builder
renderAggOp = pure . \case
  Count -> "COUNT"
  Avg   -> "AVG"
  Sum   -> "SUM"
  Min   -> "MIN"
  Max   -> "MAX"

renderExtractField :: MySQLExtractFieldSyntax -> RenderM Builder
renderExtractField = pure . \case
  SecondsField -> "SECOND"
  MinutesField -> "MINUTE"
  HourField    -> "HOUR"
  DayField     -> "DAY"
  MonthField   -> "MONTH"
  YearField    -> "YEAR"

renderCastTarget :: MySQLDataTypeSyntax -> RenderM Builder
renderCastTarget = pure . \case
  DomainType t          -> backtickWrap . textUtf8 $ t
  CharType{}            -> "CHAR"
  VarCharType{}         -> "CHAR"
  NationalCharType{}    -> "CHAR"
  NationalVarCharType{} -> "CHAR"
  BitType{}             -> "BINARY"
  VarBitType{}          -> "BINARY"
  NumericType mPrec     -> "DECIMAL" <> renderNumPrec mPrec
  DecimalType mPrec     -> "DECIMAL" <> renderNumPrec mPrec
  IntType               -> "INTEGER"
  SmallIntType          -> "INTEGER"
  FloatType prec        -> "FLOAT" <> foldMap (bracketWrap . wordDec) prec
  DoubleType            -> "DECIMAL"
  RealType              -> "DECIMAL"
  DateType              -> "DATE"
  TimeType              -> "TIME"
  TimestampType         -> "DATETIME"

renderNumPrec :: MySQLPrecision -> Builder
renderNumPrec = \case
  None            -> mempty
  DigitsOnly d    -> bracketWrap . wordDec $ d
  DigitsScale d s -> bracketWrap (wordDec d <> ", " <> wordDec s)

renderPostOp :: PostOp -> RenderM Builder
renderPostOp = pure . \case
  GIsNull       -> " IS NULL"
  GIsNotNull    -> " IS NOT NULL"
  GIsTrue       -> " IS TRUE"
  GIsNotTrue    -> " IS NOT TRUE"
  GIsFalse      -> " IS FALSE"
  GIsNotFalse   -> " IS NOT FALSE"
  GIsUnknown    -> " IS UNKNOWN"
  GIsNotUnknown -> " IS NOT UNKNOWN"

renderPrefOp :: PrefOp -> RenderM Builder
renderPrefOp = pure . \case
  LNot         -> "NOT"
  NNegate      -> "-"
  TCharLength  -> "CHAR_LENGTH"
  TOctetLength -> "OCTET_LENGTH"
  BBitLength   -> "BIT_LENGTH"
  TLower       -> "LOWER"
  TUpper       -> "UPPER"
  TTrim        -> "TRIM"
  NAbs         -> "ABS"

renderCompOp :: CompOp -> RenderM Builder
renderCompOp = pure . \case
  CEq -> "="
  CNe -> "<>"
  CLe -> "<="
  CLt -> "<"
  CGe -> ">="
  CGt -> ">"

renderQuantifier :: MySQLQuantifierSyntax -> RenderM Builder
renderQuantifier = pure . \case
  Any -> "ANY"
  All -> "ALL"

renderBinOp :: BinOp -> RenderM Builder
renderBinOp = pure . \case
  LAnd      -> "AND"
  LOr       -> "OR"
  NAdd      -> "+"
  NMul      -> "*"
  NSub      -> "-"
  NDiv      -> "/"
  NMod      -> "%"
  GLike     -> "LIKE"
  GOverlaps -> "OVERLAPS"

renderFieldName :: MySQLFieldNameSyntax -> RenderM Builder
renderFieldName = pure . \case
  UnqualifiedField f -> backtickWrap . textUtf8 $ f
  QualifiedField t f ->
    (backtickWrap . textUtf8 $ t) <> "." <> (backtickWrap . textUtf8 $ f)

renderCaseBranch :: CaseBranch -> RenderM Builder
renderCaseBranch cb = do
  c' <- renderExpr cb.condition
  a' <- renderExpr cb.action
  pure $ "WHEN " <> c' <> " THEN " <> a'

renderValue :: MySQLValueSyntax -> RenderM Builder
renderValue = \case
  VBool b -> pure . bool "FALSE" "TRUE" $ b
  VInt8 i -> pure . int8Dec $ i
  VInt16 i -> pure . int16Dec $ i
  VInt32 i -> pure . int32Dec $ i
  VInt64 i -> pure . int64Dec $ i
  VWord8 w -> pure . word8Dec $ w
  VWord16 w -> pure . word16Dec $ w
  VWord32 w -> pure . word32Dec $ w
  VWord64 w -> pure . word64Dec $ w
  VScientific s ->
    pure .
    lazyByteString .
    Bin.toLazyByteString .
    formatScientificBuilder Fixed Nothing $ s
  VFloat f -> pure . floatDec $ f
  VDouble d -> pure . doubleDec $ d
  VNothing -> pure "NULL"
  VNull -> pure "NULL"
  VByteString b -> pure . quoteWrap . byteString . escapeBytes $ b
  VText t -> escape t
  VDay d -> escape . pack . formatTime defaultTimeLocale "%F" $ d
  VLocalTime lt -> escape . pack . formatTime defaultTimeLocale "%F %T%Q" $ lt
  VTimeOfDay tod -> escape . pack . formatTime defaultTimeLocale "%T%Q" $ tod
  VViaJSON v -> escape . decodeLatin1 . toStrict . encode $ v

renderGrouping :: MySQLGroupingSyntax -> RenderM Builder
renderGrouping (GroupByExpressions es) =
  intersperse ", " . toList <$> traverse renderExpr es

invalidOp :: Text -> MySQLSelectTableSyntax -> RenderM Builder
invalidOp t = throwError . RenderError (UnsupportedOperation t) . pack . show

renderOrdering :: MySQLOrderingSyntax -> RenderM Builder
renderOrdering = \case
  AscOrdering e -> do
    e' <- renderExpr e
    pure (e' <> " ASC")
  DescOrdering e -> do
    e' <- renderExpr e
    pure (e' <> " DESC")

renderInsert' :: MySQLInsert -> RenderM Builder
renderInsert' ins = do
  tableName' <- renderTableName ins.tableName
  insertValues' <- renderInsertValues ins.insertValues
  pure $
    "INSERT INTO " <>
    tableName' <>
    " " <>
    (bracketWrap . intersperse ", " . toList . fmap (backtickWrap . textUtf8) $ ins.columns) <>
    insertValues'

renderInsertValues :: MySQLInsertValuesSyntax -> RenderM Builder
renderInsertValues = \case
  InsertSQLExpressions rti -> do
    rti' <- traverse renderTableRow rti
    pure $ " VALUES " <> (intersperse ", " . toList . fmap bracketWrap $ rti')
  InsertFromSQL sel -> renderSelect' sel

renderUpdate' :: MySQLUpdate -> RenderM Builder
renderUpdate' upd = do
  tableName' <- renderTableName upd.tableName
  updates' <- traverse renderFieldUpdate upd.updates
  wher' <- traverse renderExpr upd.wher
  pure $
    "UPDATE " <>
    tableName' <>
    go updates' <>
    foldMap (" WHERE " <>) wher'
  where
    go :: Vector Builder -> Builder
    go v = if Data.Vector.length v == 0
      then mempty
      else " SET " <> (intersperse ", " . toList $ v)

renderFieldUpdate :: FieldUpdate -> RenderM Builder
renderFieldUpdate fu = do
  fieldName' <- renderFieldName fu.fieldName
  fieldExpression' <- renderExpr fu.fieldExpression
  pure $ fieldName' <> "=" <> fieldExpression'

renderDelete' :: MySQLDelete -> RenderM Builder
renderDelete' del = do
  tableName' <- renderTableName del.tableName
  wher' <- traverse renderExpr del.wher
  pure $
    "DELETE FROM " <>
    tableName' <>
    foldMap (" WHERE " <>) wher'

wrap :: (Semigroup s) => s -> s -> s -> s
wrap l r v = l <> v <> r

bracketWrap :: (IsString s, Semigroup s) => s -> s
bracketWrap = wrap "(" ")"

quoteWrap :: (IsString s, Semigroup s) => s -> s
quoteWrap = wrap "'" "'"

escape :: Text -> RenderM Builder
escape = pure . quoteWrap . textUtf8 . escapeText

backtickWrap :: (IsString s, Semigroup s) => s -> s
backtickWrap = wrap "`" "`"
