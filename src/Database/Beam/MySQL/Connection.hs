-- Due to RDP plugin. - Koz
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy         #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeOperators #-}

module Database.Beam.MySQL.Connection where

import           Control.Exception.Safe (Exception, Handler (Handler),
                                         MonadCatch, MonadMask, MonadThrow,
                                         bracket, catchAny, catches, throw)
import           Control.Monad (void)
import           Control.Monad.Free.Church (F, iterM)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.RWS.Strict (RWST, evalRWST)
import           Control.Monad.Reader (MonadReader (ask), ReaderT (..), asks,
                                       runReaderT)
import           Control.Monad.State.Strict (MonadState (get), modify)
import           Control.Monad.Writer (execWriter, tell)
import           Data.Aeson (FromJSON)
import           Data.ByteString (ByteString)
import           Data.FakeUTC (FakeUTC)
import           Data.HashSet (HashSet)
import           Data.Int (Int16, Int32, Int64, Int8)
import           Data.Kind (Type)
import           Data.Proxy (Proxy (Proxy))
import           Data.Scientific (Scientific)
import           Data.Text (Text, pack)
import qualified Data.Text.Encoding as Enc
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Data.Time (Day, LocalTime, TimeOfDay)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.ViaJson (ViaJson)
import           Data.Word (Word16, Word32, Word64, Word8)
import           Database.Beam.Backend
import qualified Database.Beam.Backend.SQL.BeamExtensions as Beam
import           Database.Beam.Schema.Tables ( Beamable
                                             , Columnar'(..)
                                             , DatabaseEntity(..)
                                             , DatabaseEntityDescriptor(..)
                                             , TableEntity
                                             , TableField(..)
                                             , changeBeamRep )
import           Database.Beam.Query.Internal
import           Database.Beam.MySQL.FromField.DecodeError (DecodeError (DecodeError),
                                                            Lenient (IEEEInfinity, IEEENaN, IEEETooBig, IEEETooSmall, SomeStrict, TextCouldNotParse),
                                                            Strict (NotValidJSON, TypeMismatch, UnexpectedNull, Won'tFit))
import           Database.Beam.MySQL.Syntax.Render (RenderError (..),
                                                    RenderErrorType (..),
                                                    renderDelete, renderInsert,
                                                    renderSelect, renderUpdate)
import           Database.Beam.MySQL.Utils (toSQLTypeName)
#ifdef LENIENT
import           Database.Beam.MySQL.FromField.Lenient (FromField (fromField))
#else
import           Database.Beam.MySQL.FromField.Strict (FromField (fromField))
#endif
import           Database.Beam.MySQL.Syntax (MySQLSyntax (..))
import           Database.Beam.MySQL.Syntax.Insert ( MySQLInsert(InsertStmt)
                                                    , MySQLInsertOnConflictAction(..)
                                                    , MySQLInsertOnConflictTarget(..) )
import           Database.Beam.MySQL.Syntax.Update ( FieldUpdate(FieldUpdate) )
import           Database.Beam.Query ( SqlInsert(..), SqlInsertValues(..)
                                     , HasQBuilder(..), HasSqlEqualityCheck
                                     , HasSqlQuantifiedEqualityCheck)
import           Database.Beam.Query.SQL92 (buildSql92Query')
import           Database.MySQL.Base (ColumnDef, MySQLConn, MySQLValue (..),
                                      Query (..), columnOrigName, columnType,
                                      execute_, queryVector_, skipToEof)
import           Prelude hiding (mapM, read)
import           System.IO.Streams (InputStream, read)
import           System.IO.Streams.Combinators (mapM)
import qualified System.IO.Streams.List as S
import           Type.Reflection (TyCon, Typeable, tyConName)

-- | Backend type for MySQL and MariaDB.
--
-- @since 1.2.2.0
data MySQL = MySQL

-- | @since 1.2.2.0
instance BeamSqlBackendIsString MySQL String

-- | @since 1.2.2.0
instance BeamSqlBackendIsString MySQL Text

-- | @since 1.3.0.0
instance BeamBackend MySQL where
  type BackendFromField MySQL = FromField

-- | @since 1.2.2.0
instance BeamSqlBackend MySQL

-- | @since 1.2.2.0
type instance BeamSqlBackendSyntax MySQL = MySQLSyntax

-- | @since 1.2.2.0
instance HasQBuilder MySQL where
  buildSqlQuery = buildSql92Query' True

-- Instances for all types we can read using this library from the database

-- | @since 1.2.2.0
instance FromBackendRow MySQL Bool

-- | @since 1.2.2.0
instance HasSqlEqualityCheck MySQL Bool

-- | @since 1.2.2.0
instance HasSqlQuantifiedEqualityCheck MySQL Bool

-- | @since 1.2.2.0
instance FromBackendRow MySQL Int8

-- | @since 1.2.2.0
instance HasSqlEqualityCheck MySQL Int8

-- | @since 1.2.2.0
instance HasSqlQuantifiedEqualityCheck MySQL Int8

-- | @since 1.2.2.0
instance FromBackendRow MySQL Int16

-- | @since 1.2.2.0
instance HasSqlEqualityCheck MySQL Int16

-- | @since 1.2.2.0
instance HasSqlQuantifiedEqualityCheck MySQL Int16

-- | @since 1.2.2.0
instance FromBackendRow MySQL Int32

-- | @since 1.2.2.0
instance HasSqlEqualityCheck MySQL Int32

-- | @since 1.2.2.0
instance HasSqlQuantifiedEqualityCheck MySQL Int32

-- | @since 1.2.2.0
instance FromBackendRow MySQL Int64

-- | @since 1.2.2.0
instance HasSqlEqualityCheck MySQL Int64

-- | @since 1.2.2.0
instance HasSqlQuantifiedEqualityCheck MySQL Int64

-- | @since 1.2.2.0
instance FromBackendRow MySQL Int

-- | @since 1.2.2.0
instance HasSqlEqualityCheck MySQL Int

-- | @since 1.2.2.0
instance HasSqlQuantifiedEqualityCheck MySQL Int

-- | @since 1.2.2.0
instance FromBackendRow MySQL Word8

-- | @since 1.2.2.0
instance HasSqlEqualityCheck MySQL Word8

-- | @since 1.2.2.0
instance HasSqlQuantifiedEqualityCheck MySQL Word8

-- | @since 1.2.2.0
instance FromBackendRow MySQL Word16

-- | @since 1.2.2.0
instance HasSqlEqualityCheck MySQL Word16

-- | @since 1.2.2.0
instance HasSqlQuantifiedEqualityCheck MySQL Word16

-- | @since 1.2.2.0
instance FromBackendRow MySQL Word32

-- | @since 1.2.2.0
instance HasSqlEqualityCheck MySQL Word32

-- | @since 1.2.2.0
instance HasSqlQuantifiedEqualityCheck MySQL Word32

-- | @since 1.2.2.0
instance FromBackendRow MySQL Word64

-- | @since 1.2.2.0
instance HasSqlEqualityCheck MySQL Word64

-- | @since 1.2.2.0
instance HasSqlQuantifiedEqualityCheck MySQL Word64

-- | @since 1.2.2.0
instance FromBackendRow MySQL Word

-- | @since 1.2.2.0
instance HasSqlEqualityCheck MySQL Word

-- | @since 1.2.2.0
instance HasSqlQuantifiedEqualityCheck MySQL Word

-- | @since 1.2.2.0
instance FromBackendRow MySQL Float

-- | @since 1.2.2.0
instance HasSqlEqualityCheck MySQL Float

-- | @since 1.2.2.0
instance HasSqlQuantifiedEqualityCheck MySQL Float

-- | @since 1.2.2.0
instance FromBackendRow MySQL Double

-- | @since 1.2.2.0
instance HasSqlEqualityCheck MySQL Double

-- | @since 1.2.2.0
instance HasSqlQuantifiedEqualityCheck MySQL Double

-- | @since 1.2.2.0
instance FromBackendRow MySQL Scientific

-- | @since 1.2.2.0
instance HasSqlEqualityCheck MySQL Scientific

-- | @since 1.2.2.0
instance HasSqlQuantifiedEqualityCheck MySQL Scientific

-- | @since 1.2.2.0
instance FromBackendRow MySQL SqlNull

-- | @since 1.2.2.0
instance HasSqlEqualityCheck MySQL SqlNull

-- | @since 1.2.2.0
instance HasSqlQuantifiedEqualityCheck MySQL SqlNull

-- | @since 1.2.2.0
instance FromBackendRow MySQL ByteString

-- | @since 1.2.2.0
instance HasSqlEqualityCheck MySQL ByteString

-- | @since 1.2.2.0
instance HasSqlQuantifiedEqualityCheck MySQL ByteString

-- | @since 1.2.2.0
instance FromBackendRow MySQL Text

-- | @since 1.2.2.0
instance HasSqlEqualityCheck MySQL Text

-- | @since 1.2.2.0
instance HasSqlQuantifiedEqualityCheck MySQL Text

-- | @since 1.2.2.0
instance FromBackendRow MySQL LocalTime

-- | @since 1.2.2.0
instance HasSqlEqualityCheck MySQL LocalTime

-- | @since 1.2.2.0
instance HasSqlQuantifiedEqualityCheck MySQL LocalTime

-- | @since 1.2.2.0
instance FromBackendRow MySQL Day

-- | @since 1.2.2.0
instance HasSqlEqualityCheck MySQL Day

-- | @since 1.2.2.0
instance HasSqlQuantifiedEqualityCheck MySQL Day

-- | @since 1.2.2.0
instance FromBackendRow MySQL TimeOfDay

-- | @since 1.2.2.0
instance HasSqlEqualityCheck MySQL TimeOfDay

-- | @since 1.2.2.0
instance HasSqlQuantifiedEqualityCheck MySQL TimeOfDay

-- | @since 1.2.2.0
instance (Typeable a, FromJSON a) => FromBackendRow MySQL (ViaJson a)

-- | @since 1.2.2.0
instance (Typeable a, FromJSON a) => HasSqlEqualityCheck MySQL (ViaJson a)

-- | @since 1.2.2.0
instance (Typeable a, FromJSON a) => HasSqlQuantifiedEqualityCheck MySQL (ViaJson a)

-- | @since 1.2.2.0
instance FromBackendRow MySQL FakeUTC

-- | @since 1.2.2.0
instance HasSqlEqualityCheck MySQL FakeUTC

-- | @since 1.2.2.0
instance HasSqlQuantifiedEqualityCheck MySQL FakeUTC

instance Beam.BeamHasInsertOnConflict MySQL where
  newtype SqlConflictTarget MySQL table = MySQLConflictTarget
    { unMySQLConflictTarget :: table (QExpr MySQL QInternal) -> MySQLInsertOnConflictTarget }
  newtype SqlConflictAction MySQL table = MySQLConflictAction
    { unMySQLConflictAction :: forall s. table (QField s) -> MySQLInsertOnConflictAction }

  insertOnConflict
    :: forall db table s. Beamable table
    => DatabaseEntity MySQL db (TableEntity table)
    -> SqlInsertValues MySQL (table (QExpr MySQL s))
    -> Beam.SqlConflictTarget MySQL table
    -> Beam.SqlConflictAction MySQL table
    -> SqlInsert MySQL table
  insertOnConflict (DatabaseEntity dt) vals _ action = case vals of
    SqlInsertValuesEmpty -> SqlInsertNoRows
    SqlInsertValues vs   -> SqlInsert (dbTableSettings dt) $
      let getFieldName
            :: forall a
            .  Columnar' (TableField table) a
            -> Columnar' (QField QInternal) a
          getFieldName (Columnar' fd) = Columnar' $ QField False (dbTableCurrentName dt) $ _fieldName fd
          tableFields = changeBeamRep getFieldName $ dbTableSettings dt
          tellFieldName _ _ f = tell [f] >> pure f
          fieldNames = execWriter $ project' (Proxy @AnyType) (Proxy @((), Text)) tellFieldName tableFields
        in InsertStmt (tableNameFromEntity dt) (V.fromList fieldNames) vs (Just $ unMySQLConflictAction action tableFields)

  anyConflict = MySQLConflictTarget (const MySQLInsertOnConflictAnyTarget)
  conflictingFields _ = error "MySQL does not support CONFLICT_TARGETS"
  conflictingFieldsWhere _ _ = error "MySQL does not support CONFLICT_TARGETS with WHERE"

  onConflictDoNothing = MySQLConflictAction (const IGNORE)
  onConflictUpdateSet makeAssignments =
    MySQLConflictAction $ \table ->
      let QAssignment assignments = makeAssignments table fieldsAliased
          fieldsAliased = changeBeamRep (\(Columnar' (QField _ _ nm)) -> Columnar' (QExpr (\_ -> fieldE (qualifiedField "new_values" nm)))) table
      in UPDATE_ON_DUPLICATE_KEY $ V.fromList [FieldUpdate fieldNm expr | (fieldNm, expr) <- assignments]
  onConflictUpdateSetWhere _ _ = error "MySQL does not support UPDATE_SET with WHERE"

-- | Some possible (but predictable) failure modes of this backend.
--
-- @since 1.2.2.0
data MySQLStatementError =
  -- | This back-end does not support a given operation in this context.
  --
  -- @since 1.2.2.0
  OperationNotSupported {
    operationName :: {-# UNPACK #-} !Text,
    context       :: {-# UNPACK #-} !Text,
    statement     :: {-# UNPACK #-} !Text
    } |
  -- | This expression would be impure, and therefore cannot be safely replayed.
  -- This can arise when using 'runInsertRowReturning', as we have to simulate
  -- this functionality on older versions of MySQL, requiring us to be able to
  -- replay expressions.
  --
  -- @since 1.2.2.0
  ImpureExpression {
    expression :: {-# UNPACK #-} !Text,
    statement  :: {-# UNPACK #-} !Text
    }
  deriving stock (
                  Eq -- ^ @since 1.2.2.0
                  , Show -- ^ @since 1.2.2.0
                  )

-- | @since 1.2.2.0
instance Exception MySQLStatementError

-- | Represents various forms of failure on column decodes.
--
-- @since 1.3.0.0
data ColumnDecodeError =
  -- | We received a @NULL@ for a column that isn't @NULLABLE@.
  --
  -- @since 1.3.0.0
  FoundUnexpectedNull {
    demandedType   :: {-# UNPACK #-} !Text,
    sqlType        :: {-# UNPACK #-} !Text,
    tablesInvolved :: !(HashSet Text),
    columnName     :: {-# UNPACK #-} !Text
    } |
  -- | We cannot decode the given SQL type into the demanded Haskell type.
  --
  -- @since 1.3.0.0
  Can'tDecodeIntoDemanded {
    demandedType   :: {-# UNPACK #-} !Text,
    sqlType        :: {-# UNPACK #-} !Text,
    tablesInvolved :: !(HashSet Text),
    columnName     :: {-# UNPACK #-} !Text,
    value          :: {-# UNPACK #-} !Text
    } |
  -- | The SQL value provided by the database does not fit into the
  -- representation of the demanded Haskell type.
  --
  -- @since 1.3.0.0
  ValueWon'tFitIntoType {
    demandedType   :: {-# UNPACK #-} !Text,
    sqlType        :: {-# UNPACK #-} !Text,
    tablesInvolved :: !(HashSet Text),
    columnName     :: {-# UNPACK #-} !Text,
    value          :: {-# UNPACK #-} !Text
    } |
  -- | When parsing leniently, we found a NaN. As the demanded type is allowed
  -- to vary considerably, we cannot handle NaNs in general; in the cases where
  -- we can't, this error arises.
  --
  -- /See also:/ @LENIENT.md@
  --
  -- @since 1.3.0.0
  LenientUnexpectedNaN {
    demandedType   :: {-# UNPACK #-} !Text,
    sqlType        :: {-# UNPACK #-} !Text,
    tablesInvolved :: !(HashSet Text),
    columnName     :: {-# UNPACK #-} !Text
    } |
  -- | When parsing leniently, we found a negative or positive infinity. As the
  -- demanded type is allowed to vary considerably, we cannot handle this case
  -- in general; in those situations where we can't, this error arises.
  --
  -- /See also:/ @LENIENT.md@
  --
  -- @since 1.3.0.0
  LenientUnexpectedInfinity {
    demandedType   :: {-# UNPACK #-} !Text,
    sqlType        :: {-# UNPACK #-} !Text,
    tablesInvolved :: !(HashSet Text),
    columnName     :: {-# UNPACK #-} !Text,
    value          :: {-# UNPACK #-} !Text
    } |
  -- | When parsing leniently, we found a value that's too negative to fit into
  -- the representation requested.
  --
  -- /See also:/ @LENIENT.md@
  --
  -- @since 1.3.0.0
  LenientTooSmallToFit {
    demandedType   :: {-# UNPACK #-} !Text,
    sqlType        :: {-# UNPACK #-} !Text,
    tablesInvolved :: !(HashSet Text),
    columnName     :: {-# UNPACK #-} !Text,
    value          :: {-# UNPACK #-} !Text
    } |
  -- | When parsing leniently, we found a value that's too large (that is, too
  -- far along the number line in the positive direction) to fit into the
  -- representation requested.
  --
  -- /See also:/ @LENIENT.md@
  --
  -- @since 1.2.2.0
  LenientTooBigToFit {
    demandedType   :: {-# UNPACK #-} !Text,
    sqlType        :: {-# UNPACK #-} !Text,
    tablesInvolved :: !(HashSet Text),
    columnName     :: {-# UNPACK #-} !Text,
    value          :: {-# UNPACK #-} !Text
    } |
  -- | When parsing leniently, we could not decode a value of the requested type
  -- from a textual representation.
  --
  -- /See also:/ @LENIENT.md@
  --
  -- @since 1.3.0.0
  LenientTextCouldn'tParse {
    demandedType   :: {-# UNPACK #-} !Text,
    sqlType        :: {-# UNPACK #-} !Text,
    tablesInvolved :: !(HashSet Text),
    columnName     :: {-# UNPACK #-} !Text,
    value          :: {-# UNPACK #-} !Text
    } |
  -- | There were not enough fields returned by the database to satisfy the
  -- requested type.
  --
  -- @since 1.3.0.0
  DemandedTooManyFields {
    fieldsAvailable :: {-# UNPACK #-} !Int,
    fieldsDemanded  :: {-# UNPACK #-} !Int
    } |
  -- | When using the 'ViaJson' newtype wrapper to aid parsing, something went
  -- wrong with parsing the JSON representation of the desired value.
  --
  -- @since 1.3.0.0
  JSONError {
    demandedType   :: {-# UNPACK #-} !Text,
    sqlType        :: {-# UNPACK #-} !Text,
    tablesInvolved :: !(HashSet Text),
    columnName     :: {-# UNPACK #-} !Text,
    value          :: {-# UNPACK #-} !Text
    }
  deriving stock (
                  Eq -- ^ @since 1.2.2.0
                  , Show -- ^ @since 1.2.2.0
                  )

-- | @since 1.2.2.0
instance Exception ColumnDecodeError

-- Small helper for our environment
data MySQLMEnv =
  DebugEnv !(Text -> IO ())
           {-# UNPACK #-} !MySQLConn |
  ReleaseEnv {-# UNPACK #-} !MySQLConn

-- | Operational monad for MySQL/MariaDB Beam actions.
--
-- @since 1.2.2.0
newtype MySQLM (a :: Type) = MySQLM (ReaderT MySQLMEnv IO a)
  deriving (Functor -- ^ @since 1.2.2.0
            , Applicative -- ^ @since 1.2.2.0
            , Monad -- ^ @since 1.2.2.0
            , MonadIO -- ^ @since 1.2.2.0
            , MonadMask -- ^ @since 1.2.2.0
            , MonadCatch -- ^ @since 1.2.2.0
            , MonadThrow -- ^ @since 1.2.2.0
            ) via (ReaderT MySQLMEnv IO)

-- | @since 1.2.2.0
instance MonadFail MySQLM where
  fail err = error ("Internal error with: " <> err)

-- | @since 1.3.0.0
instance MonadBeam MySQL MySQLM where
  {-# INLINEABLE runNoReturn #-}
  runNoReturn sql = do
    (stmt, conn, _) <- processAndLog sql
    void . liftIO . execute_ conn $ stmt
  {-# INLINEABLE runReturningOne #-}
  runReturningOne sql = do
    (stmt, conn, tables) <- processAndLog sql
    bracket (acquireStream conn stmt)
            drainStream
            (processOneResult tables)
  {-# INLINEABLE runReturningList #-}
  runReturningList sql = do
    (stmt, conn, tables) <- processAndLog sql
    bracket (acquireStream conn stmt)
            drainStream
            (\(env, stream) -> liftIO $ S.toList =<< parseRowsIO env tables stream)
  {-# INLINEABLE runReturningMany #-}
  runReturningMany sql callback = do
    (stmt, conn, tables) <- processAndLog sql
    bracket (acquireStream conn stmt)
            drainStream
            (\(env, stream) -> callback $ liftIO (read =<< parseRowsIO env tables stream))

-- | Run a series of MySQL operations without debugging.
--
-- @since 1.2.2.0
runBeamMySQL :: MySQLConn -> MySQLM a -> IO a
runBeamMySQL conn (MySQLM comp) = runReaderT comp . ReleaseEnv $ conn

-- | Run a series of MySQL operations with debug output provided by the handler.
--
-- @since 1.2.2.0
runBeamMySQLDebug :: (Text -> IO ()) -> MySQLConn -> MySQLM a -> IO a
runBeamMySQLDebug dbg conn (MySQLM comp) =
  runReaderT comp . DebugEnv dbg $ conn

-- Helpers

-- Rendering aid

renderMySQL :: MySQLSyntax -> Either RenderError (Query, HashSet Text)
renderMySQL = \case
  ASelect sql  -> renderSelect sql
  AnInsert sql -> renderInsert sql
  AnUpdate sql -> renderUpdate sql
  ADelete sql  -> renderDelete sql

-- Removes some duplication from MonadBeam instance

processAndLog :: MySQLSyntax -> MySQLM (Query, MySQLConn, HashSet Text)
processAndLog sql = do
  let rendered = renderMySQL sql
  case rendered of
    Left (RenderError typ ast) -> case typ of
      UnsupportedOperation op ->
        throw (OperationNotSupported op ast (pack . show $ sql))
    Right (stmt@(Query inner), tables) -> do
      env <- MySQLM ask
      conn <- case env of
        DebugEnv dbg conn -> do
          let textual = toStrict . decodeUtf8 $ inner
          liftIO (dbg textual) >> pure conn
        ReleaseEnv conn -> pure conn
      pure (stmt, conn, tables)

acquireStream :: (MonadIO m) =>
  MySQLConn -> Query -> m (Vector ColumnDef, InputStream (Vector MySQLValue))
acquireStream conn = liftIO . queryVector_ conn

drainStream :: (MonadIO m) =>
  (Vector ColumnDef, InputStream (Vector MySQLValue)) -> m ()
drainStream (_, stream) = liftIO . skipToEof $ stream

processOneResult :: forall (a :: Type) .
  (FromBackendRow MySQL a) =>
  HashSet Text ->
  (Vector ColumnDef, InputStream (Vector MySQLValue)) ->
  MySQLM (Maybe a)
processOneResult tables (env, stream) = do
  mRes <- liftIO . read $ stream
  case mRes of
    Nothing  -> pure Nothing
    Just res -> Just <$> runDecode (iterM (decodeFromRow needed) churched) res tables env
  where
    churched :: F (FromBackendRowF MySQL) a
    FromBackendRowM churched = fromBackendRow
    needed :: Int
    needed = valuesNeeded (Proxy @MySQL) $ Proxy @a

parseRowsIO :: forall (a :: Type) .
  (FromBackendRow MySQL a) =>
  Vector ColumnDef ->
  HashSet Text ->
  InputStream (Vector MySQLValue) ->
  IO (InputStream a)
parseRowsIO env tables = mapM go
  where
    go :: Vector MySQLValue -> IO a
    go v = runDecode (iterM (decodeFromRow needed) churched) v tables env
    churched :: F (FromBackendRowF MySQL) a
    FromBackendRowM churched = fromBackendRow
    needed :: Int
    needed = valuesNeeded (Proxy @MySQL) $ Proxy @a

-- Decoding from a row is complex enough to warrant its own operators and an
-- explicit stack.

data DataExhaustedError = DataExhaustedError {
  available :: {-# UNPACK #-} !Int,
  demanded  :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Show)

instance Exception DataExhaustedError

data LenientDecodeError = LenientDecodeError Int (DecodeError Lenient)
  deriving stock (Show, Eq)

instance Exception LenientDecodeError

data StrictDecodeError = StrictDecodeError Int (DecodeError Strict)
  deriving stock (Show, Eq)

instance Exception StrictDecodeError

data DecodeEnv = DecodeEnv {
  columnInfo :: {-# UNPACK #-} !(Vector ColumnDef),
  actualData :: {-# UNPACK #-} !(Vector MySQLValue)
  }

newtype Decode (m :: Type -> Type) (a :: Type) =
  Decode (RWST DecodeEnv () Int m a)
  deriving (Functor,
            Applicative,
            Monad,
            MonadReader DecodeEnv,
            MonadState Int,
            MonadThrow,
            MonadCatch) via (RWST DecodeEnv () Int m)

runDecode :: forall (m :: Type -> Type) (a :: Type) .
  (MonadCatch m) =>
  Decode m a ->
  Vector MySQLValue ->
  HashSet Text ->
  Vector ColumnDef ->
  m a
runDecode (Decode comp) v tables env =
  catches (fst <$> evalRWST comp (DecodeEnv env v) 0)
          [Handler handleDataExhausted,
          Handler handleLenient,
          Handler handleStrict]
  where
    handleDataExhausted :: DataExhaustedError -> m a
    handleDataExhausted err =
      throw . DemandedTooManyFields err.available $ err.demanded
    handleLenient :: LenientDecodeError -> m a
    handleLenient (LenientDecodeError lastIx (DecodeError err typ)) = do
      let ft = toSQLTypeName . columnType $ env V.! lastIx
      let tyName = tyConNameText typ
      let colName' = Enc.decodeUtf8 . columnOrigName $ env V.! lastIx
      let v' = pack . show $ v V.! lastIx
      case err of
        SomeStrict err' -> handleStrict . StrictDecodeError lastIx . DecodeError err' $ typ
        IEEENaN -> throw . LenientUnexpectedNaN tyName ft tables $ colName'
        IEEEInfinity -> throw . LenientUnexpectedInfinity tyName ft tables colName' $ v'
        IEEETooSmall -> throw . LenientTooSmallToFit tyName ft tables colName' $ v'
        IEEETooBig -> throw . LenientTooBigToFit tyName ft tables colName' $ v'
        TextCouldNotParse -> throw . LenientTextCouldn'tParse tyName ft tables colName' $ v'
    handleStrict :: StrictDecodeError -> m a
    handleStrict (StrictDecodeError lastIx (DecodeError err typ)) = do
      let ft = toSQLTypeName . columnType $ env V.! lastIx
      let tyName = tyConNameText typ
      let colName' = Enc.decodeUtf8 . columnOrigName $ env V.! lastIx
      let v' = pack . show $ v V.! lastIx
      throw $ case err of
        UnexpectedNull -> FoundUnexpectedNull tyName ft tables colName'
        TypeMismatch   -> Can'tDecodeIntoDemanded tyName ft tables colName' v'
        Won'tFit       -> ValueWon'tFitIntoType tyName ft tables colName' v'
        NotValidJSON   -> JSONError tyName ft tables colName' v'

decodeFromRow :: forall (m :: Type -> Type) (a :: Type) .
  (MonadCatch m) =>
  Int -> FromBackendRowF MySQL (Decode m a) -> Decode m a
decodeFromRow needed = \case
  ParseOneField callback                                     -> do
    curr <- currentValue
    case curr of
      Nothing  -> do
        ix <- get
        throw . DataExhaustedError (ix + 1) $ needed
      Just val -> case fromField val of
        Left err -> do
          ix <- get
          errBuild ix err
        Right x  -> advanceIndex >> callback x
  Alt (FromBackendRowM opt1) (FromBackendRowM opt2) callback -> do
                -- The 'catch-in-catch' here is needed due to the peculiar way
                -- in which beam parses NULLable columns. Essentially, it first
                -- tries to grab a non-NULL value, then, if it fails, tries to
                -- unconditionally grab a NULL.
    res1 <- catchAny ((pure . Right) =<< iterM (decodeFromRow needed) opt1)
                      (pure . Left)
    case res1 of
      Right r1 -> callback r1
      Left err1 -> do
        res2 <- catchAny ((pure . Right) =<< iterM (decodeFromRow  needed) opt2)
                          (pure . Left)
        case res2 of
          Right r2   -> callback r2
                -- This is encoded as an Alt. Therefore, if we don't want
                -- strange false positives regarding NULL parses, we have to
                -- forward the _first_ error we saw.
          Left _err2 -> throw err1
  FailParseWith err                                          ->
    error $ "Leaked beam internal with:" <> show err
  where
#ifdef LENIENT
    errBuild :: Int -> DecodeError Lenient -> Decode m a
    errBuild ix = throw . LenientDecodeError ix
#else
    errBuild :: Int -> DecodeError Strict -> Decode m a
    errBuild ix = throw . StrictDecodeError ix
#endif

currentValue :: (Monad m) => Decode m (Maybe MySQLValue)
currentValue = do
  ix <- get
  asks ((V.!? ix) . (.actualData))

advanceIndex :: (Monad m) => Decode m ()
advanceIndex = modify succ

tyConNameText :: TyCon -> Text
tyConNameText = pack . tyConName
