{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Beam.MySQL.FromField.Strict where

import qualified GHC.Records
import qualified GHC.Records.Extra
import           Data.Aeson (Value, FromJSON(..), decodeStrict)
import           Data.Bits (Bits, toIntegralSized, zeroBits)
import           Data.ByteString (ByteString)
import           Data.FakeUTC (FakeUTC (FakeUTC))
import           Data.Int (Int16, Int32, Int64, Int8)
import           Data.Kind (Type)
import           Data.Scientific (Scientific, toBoundedInteger)
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import           Data.Time (Day, LocalTime (LocalTime), TimeOfDay,
                            localTimeToUTC, midnight, utc)
import           Data.ViaJson (ViaJson (ViaJson))
import           Data.Word (Word16, Word32, Word64, Word8)
import           Database.Beam.Backend.SQL (SqlNull (SqlNull))
import           Database.Beam.MySQL.FromField.DecodeError (DecodeError (DecodeError),
                                                            Strict (NotValidJSON, TypeMismatch, UnexpectedNull, Won'tFit))
import           Database.Beam.MySQL.TextHandling (decodeText)
import           Database.MySQL.Base (MySQLValue (..))
import           Type.Reflection (TyCon, Typeable, typeRep, typeRepTyCon)

class FromField (a :: Type) where
  fromField :: MySQLValue -> Either (DecodeError Strict) a

instance FromField Bool where
  {-# INLINEABLE fromField #-}
  fromField = \case
    MySQLInt8 v  -> pure $ zeroBits /= v
    MySQLInt8U v -> pure $ zeroBits /= v
    MySQLBit v   -> pure $ zeroBits /= v
    v            -> handleNullOrMismatch v

instance FromField Int8 where
  {-# INLINEABLE fromField #-}
  fromField = \case
    MySQLInt8 v    -> pure v
    MySQLDecimal v -> tryScientific v
    v              -> handleNullOrMismatch v

instance FromField Int16 where
  {-# INLINEABLE fromField #-}
  fromField = \case
    MySQLInt8 v    -> pure . fromIntegral $ v
    MySQLInt16 v   -> pure v
    MySQLDecimal v -> tryScientific v
    v              -> handleNullOrMismatch v

instance FromField Int32 where
  {-# INLINEABLE fromField #-}
  fromField = \case
    MySQLInt8 v    -> pure . fromIntegral $ v
    MySQLInt16 v   -> pure . fromIntegral $ v
    MySQLInt32 v   -> pure v
    MySQLDecimal v -> tryScientific v
    v              -> handleNullOrMismatch v

instance FromField Int64 where
  {-# INLINEABLE fromField #-}
  fromField = \case
    MySQLInt8 v    -> pure . fromIntegral $ v
    MySQLInt16 v   -> pure . fromIntegral $ v
    MySQLInt32 v   -> pure . fromIntegral $ v
    MySQLInt64 v   -> pure v
    MySQLDecimal v -> tryScientific v
    v              -> handleNullOrMismatch v

instance FromField Int where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLInt8 v    -> pure . fromIntegral $ v
    MySQLInt16 v   -> pure . fromIntegral $ v
    MySQLInt32 v   -> tryFixed v
    MySQLInt64 v   -> tryFixed v
    MySQLDecimal v -> tryScientific v
    v              -> handleNullOrMismatch v

instance FromField Word8 where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLInt8U v   -> pure v
    MySQLDecimal v -> tryScientific v
    v              -> handleNullOrMismatch v

instance FromField Word16 where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLInt8U v   -> pure . fromIntegral $ v
    MySQLInt16U v  -> pure v
    MySQLDecimal v -> tryScientific v
    v              -> handleNullOrMismatch v

instance FromField Word32 where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLInt8U v   -> pure . fromIntegral $ v
    MySQLInt16U v  -> pure . fromIntegral $ v
    MySQLInt32U v  -> pure v
    MySQLDecimal v -> tryScientific v
    v              -> handleNullOrMismatch v

instance FromField Word64 where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLInt8U v   -> pure . fromIntegral $ v
    MySQLInt16U v  -> pure . fromIntegral $ v
    MySQLInt32U v  -> pure . fromIntegral $ v
    MySQLInt64U v  -> pure v
    MySQLDecimal v -> tryScientific v
    v              -> handleNullOrMismatch v

instance FromField Word where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLInt8U v   -> pure . fromIntegral $ v
    MySQLInt16U v  -> pure . fromIntegral $ v
    MySQLInt32U v  -> tryFixed v
    MySQLInt64U v  -> tryFixed v
    MySQLDecimal v -> tryScientific v
    v              -> handleNullOrMismatch v

instance FromField Scientific where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLDecimal v -> pure v
    MySQLInt8 v    -> pure . fromIntegral $ v
    MySQLInt8U v   -> pure . fromIntegral $ v
    MySQLInt16 v   -> pure . fromIntegral $ v
    MySQLInt16U v  -> pure . fromIntegral $ v
    MySQLInt32 v   -> pure . fromIntegral $ v
    MySQLInt32U v  -> pure . fromIntegral $ v
    MySQLInt64 v   -> pure . fromIntegral $ v
    MySQLInt64U v  -> pure . fromIntegral $ v
    v              -> handleNullOrMismatch v

instance FromField Float where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLFloat v -> Right v
    v            -> handleNullOrMismatch v

instance FromField Double where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLFloat v  -> pure . realToFrac $ v
    MySQLDouble v -> pure v
    v             -> handleNullOrMismatch v

instance FromField SqlNull where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLNull -> pure SqlNull
    _         -> Left . DecodeError TypeMismatch $ tyCon @SqlNull

instance FromField ByteString where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLBytes v -> pure v
    v            -> handleNullOrMismatch v

instance FromField Text where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLText v -> pure . decodeText $ v
    v           -> handleNullOrMismatch v

instance FromField LocalTime where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLDateTime v  -> pure v
    MySQLTimeStamp v -> pure v
    MySQLDate v      -> pure . LocalTime v $ midnight
    v                -> handleNullOrMismatch v

instance FromField Day where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLDate v -> pure v
    v           -> handleNullOrMismatch v

instance FromField TimeOfDay where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLTime s v ->
      if s == zeroBits
      then pure v
      else
        Left . DecodeError TypeMismatch $ tyCon @TimeOfDay
    v -> handleNullOrMismatch v

instance (Typeable a, FromJSON a) => FromField (ViaJson a) where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLText v -> case decodeStrict . encodeUtf8 . decodeText $ v of
      Nothing -> Left . DecodeError NotValidJSON $ tyCon @a
      Just x  -> pure . ViaJson $ x
    MySQLJSON bs -> case decodeStrict bs of
      Nothing -> Left . DecodeError NotValidJSON $ tyCon @a
      Just x -> pure . ViaJson $ x
    v -> handleNullOrMismatch v

instance FromField FakeUTC where
  {-# INLINABLE fromField #-}
  fromField = fmap (FakeUTC . localTimeToUTC utc) . fromField

instance FromField Value where
  {-# INLINABLE fromField #-}
  fromField = \case
    MySQLJSON bs -> case decodeStrict bs of
      Nothing -> Left . DecodeError NotValidJSON $ tyCon @Value
      Just x -> pure x
    v -> handleNullOrMismatch v
-- Helpers

handleNullOrMismatch :: forall (a :: Type) .
  (Typeable a) =>
  MySQLValue -> Either (DecodeError Strict) a
handleNullOrMismatch = Left . \case
  MySQLNull -> DecodeError UnexpectedNull $ tyCon @a
  _         -> DecodeError TypeMismatch $ tyCon @a

tryScientific :: forall (a :: Type) .
  (Typeable a, Integral a, Bounded a) =>
  Scientific -> Either (DecodeError Strict) a
tryScientific s = case toBoundedInteger s of
  Nothing -> Left . DecodeError Won'tFit $ tyCon @a
  Just v  -> pure v

tyCon :: forall (a :: Type) . (Typeable a) => TyCon
tyCon = typeRepTyCon $ typeRep @a

tryFixed :: forall (b :: Type) (a :: Type) .
  (Integral a, Integral b, Bits a, Bits b, Typeable b) =>
  a -> Either (DecodeError Strict) b
tryFixed v = case toIntegralSized v of
  Nothing -> Left . DecodeError Won'tFit $ tyCon @b
  Just v' -> pure v'
