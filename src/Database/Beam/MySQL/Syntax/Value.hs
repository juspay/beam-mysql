module Database.Beam.MySQL.Syntax.Value where

import qualified GHC.Records
import qualified GHC.Records.Extra
import           Data.Aeson (ToJSON (toJSON), Value)
import           Data.ByteString (ByteString)
import           Data.FakeUTC (FakeUTC (FakeUTC))
import           Data.Int (Int16, Int32, Int64, Int8)
import           Data.Scientific (Scientific)
import           Data.Text (Text, pack)
import           Data.Time (Day, LocalTime, TimeOfDay, utc, utcToLocalTime)
import           Data.ViaJson (ViaJson (ViaJson))
import           Data.Word (Word16, Word32, Word64, Word8)
import           Database.Beam.Backend.SQL (HasSqlValueSyntax (sqlValueSyntax),
                                            SqlNull)
import           Database.Beam.MySQL.TextHandling (encodeText)

-- | Syntactic representation of a MySQL value. For convenience, these store
-- Haskell values directly, rather than their syntactic representations.
--
-- @since 1.2.3.1
data MySQLValueSyntax =
  -- | Equivalent to @BIT(1)@.
  --
  -- @since 1.2.3.1
  VBool !Bool |
  -- | Equivalent to @TINYINT@.
  --
  -- @since 1.2.3.1
  VInt8 {-# UNPACK #-} !Int8 |
  -- | Equivalent to @SMALLINT@.
  --
  -- @since 1.2.3.1
  VInt16 {-# UNPACK #-} !Int16 |
  -- | Equivalent to @INT@.
  --
  -- @since 1.2.3.1
  VInt32 {-# UNPACK #-} !Int32 |
  -- | Equivalent to @BIGINT@.
  --
  -- @since 1.2.3.1
  VInt64 {-# UNPACK #-} !Int64 |
  -- | Equivalent to @UNSIGNED TINYINT@.
  --
  -- @since 1.2.3.1
  VWord8 {-# UNPACK #-} !Word8 |
  -- | Equivalent to @UNSIGNED SMALLINT@.
  --
  -- @since 1.2.3.1
  VWord16 {-# UNPACK #-} !Word16 |
  -- | Equivalent to @UNSIGNED INT@.
  --
  -- @since 1.2.3.1
  VWord32 {-# UNPACK #-} !Word32 |
  -- | Equivalent to @UNSIGNED BIGINT@.
  --
  -- @since 1.2.3.1
  VWord64 {-# UNPACK #-} !Word64 |
  -- | Equivalent to DECIMAL, but can theoretically represent a lot of other
  -- kinds of number too.
  --
  -- @since 1.2.3.1
  VScientific {-# UNPACK #-} !Scientific |
  -- | Equivalent to @FLOAT@.
  --
  -- @since 1.2.3.1
  VFloat {-# UNPACK #-} !Float |
  -- | Equivalent to @DOUBLE@.
  --
  -- @since 1.2.3.1
  VDouble {-# UNPACK #-} !Double |
  -- | Represents a missing value. This is different to 'VNull', as it
  -- represents @NULL@ in the context of a Haskell 'Maybe'. There's no
  -- distinction between this and a @NULL@ literal at the SQL level, but we
  -- maintain this information in case we need it down the line.
  --
  -- @since 1.2.3.1
  VNothing |
  -- | Represents a literal @NULL@. This is different to 'VNothing', as the
  -- latter only occurs in Haskell 'Maybe' contexts. There's no distinction
  -- between these at the SQL level, but we maintain this information in case we
  -- need it down the line.
  --
  -- @since 1.2.3.1
  VNull |
  -- | A blob of binary. Can represent @BIT@, @VARBINARY@ and several related
  -- types.
  --
  -- @since 1.2.3.1
  VByteString {-# UNPACK #-} !ByteString |
  -- | Some text. Can represent @CHAR@, @VARCHAR@ and several related types.
  --
  -- @since 1.2.3.1
  VText {-# UNPACK #-} !Text |
  -- | A day.
  --
  -- @since 1.2.3.1
  VDay !Day |
  -- | A datetime, using whatever zone the database is in.
  --
  -- @since 1.2.3.1
  VLocalTime {-# UNPACK #-} !LocalTime |
  -- | A timestamp without a date.
  --
  -- @since 1.2.3.1
  VTimeOfDay {-# UNPACK #-} !TimeOfDay |
  -- | Support for 'ViaJson' only.
  --
  -- @since 1.2.3.1
  VViaJSON !Value
  deriving stock (
    -- | @since 1.2.3.1
    Eq
    ,
    -- | @since 1.2.3.1
    Show
    )

-- | @since 1.2.3.1
instance HasSqlValueSyntax MySQLValueSyntax Bool where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: Bool -> MySQLValueSyntax
  sqlValueSyntax = VBool

-- | @since 1.2.3.1
instance HasSqlValueSyntax MySQLValueSyntax Int8 where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: Int8 -> MySQLValueSyntax
  sqlValueSyntax = VInt8

-- | @since 1.2.3.1
instance HasSqlValueSyntax MySQLValueSyntax Int16 where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: Int16 -> MySQLValueSyntax
  sqlValueSyntax = VInt16

-- | @since 1.2.3.1
instance HasSqlValueSyntax MySQLValueSyntax Int32 where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: Int32 -> MySQLValueSyntax
  sqlValueSyntax = VInt32

-- | @since 1.2.3.1
instance HasSqlValueSyntax MySQLValueSyntax Int64 where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: Int64 -> MySQLValueSyntax
  sqlValueSyntax = VInt64

-- | @since 1.2.3.1
instance HasSqlValueSyntax MySQLValueSyntax Int where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: Int -> MySQLValueSyntax
  sqlValueSyntax = VInt64 . fromIntegral

-- | @since 1.2.3.1
instance HasSqlValueSyntax MySQLValueSyntax Word8 where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: Word8 -> MySQLValueSyntax
  sqlValueSyntax = VWord8

-- | @since 1.2.3.1
instance HasSqlValueSyntax MySQLValueSyntax Word16 where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: Word16 -> MySQLValueSyntax
  sqlValueSyntax = VWord16

-- | @since 1.2.3.1
instance HasSqlValueSyntax MySQLValueSyntax Word32 where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: Word32 -> MySQLValueSyntax
  sqlValueSyntax = VWord32

-- | @since 1.2.3.1
instance HasSqlValueSyntax MySQLValueSyntax Word64 where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: Word64 -> MySQLValueSyntax
  sqlValueSyntax = VWord64

-- | @since 1.2.3.1
instance HasSqlValueSyntax MySQLValueSyntax Word where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: Word -> MySQLValueSyntax
  sqlValueSyntax = VWord64 . fromIntegral

-- | @since 1.2.3.1
instance HasSqlValueSyntax MySQLValueSyntax Scientific where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: Scientific -> MySQLValueSyntax
  sqlValueSyntax = VScientific

-- | @since 1.2.3.1
instance HasSqlValueSyntax MySQLValueSyntax Float where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: Float -> MySQLValueSyntax
  sqlValueSyntax = VFloat

-- | @since 1.2.3.1
instance HasSqlValueSyntax MySQLValueSyntax Double where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: Double -> MySQLValueSyntax
  sqlValueSyntax = VDouble

-- | @since 1.2.3.1
instance (HasSqlValueSyntax MySQLValueSyntax a) =>
  HasSqlValueSyntax MySQLValueSyntax (Maybe a) where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: Maybe a -> MySQLValueSyntax
  sqlValueSyntax = maybe VNothing sqlValueSyntax

-- | @since 1.2.3.1
instance HasSqlValueSyntax MySQLValueSyntax SqlNull where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: SqlNull -> MySQLValueSyntax
  sqlValueSyntax = const VNull

-- | @since 1.2.3.1
instance HasSqlValueSyntax MySQLValueSyntax ByteString where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: ByteString -> MySQLValueSyntax
  sqlValueSyntax = VByteString

-- | @since 1.2.3.1
instance HasSqlValueSyntax MySQLValueSyntax Text where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: Text -> MySQLValueSyntax
  sqlValueSyntax = VText . encodeText

-- | @since 1.2.3.1
instance HasSqlValueSyntax MySQLValueSyntax String where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: String -> MySQLValueSyntax
  sqlValueSyntax = VText . encodeText . pack

-- | @since 1.2.3.1
instance HasSqlValueSyntax MySQLValueSyntax Day where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: Day -> MySQLValueSyntax
  sqlValueSyntax = VDay

-- | @since 1.2.3.1
instance HasSqlValueSyntax MySQLValueSyntax TimeOfDay where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: TimeOfDay -> MySQLValueSyntax
  sqlValueSyntax = VTimeOfDay

-- | @since 1.2.3.1
instance HasSqlValueSyntax MySQLValueSyntax LocalTime where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: LocalTime -> MySQLValueSyntax
  sqlValueSyntax = VLocalTime

-- | @since 1.2.3.1
instance (ToJSON a) => HasSqlValueSyntax MySQLValueSyntax (ViaJson a) where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: ViaJson a -> MySQLValueSyntax
  sqlValueSyntax (ViaJson v) = VViaJSON . toJSON $ v

-- | @since 1.2.3.1
instance HasSqlValueSyntax MySQLValueSyntax FakeUTC where
  {-# INLINABLE sqlValueSyntax #-}
  sqlValueSyntax :: FakeUTC -> MySQLValueSyntax
  sqlValueSyntax (FakeUTC t) = VLocalTime . utcToLocalTime utc $ t
