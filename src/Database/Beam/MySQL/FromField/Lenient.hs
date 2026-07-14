{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Beam.MySQL.FromField.Lenient where

import qualified GHC.Records
import qualified GHC.Records.Extra
import           Data.Aeson (FromJSON, decodeStrict)
import           Data.ByteString (ByteString)
import           Data.FakeUTC (FakeUTC)
import           Data.Int (Int16, Int32, Int64, Int8)
import           Data.Kind (Type)
import           Data.Scientific (Scientific)
import           Data.Text (Text, pack, unpack)
import           Data.Time (Day, LocalTime, TimeOfDay)
import           Data.ViaJson (ViaJson (ViaJson))
import           Data.Word (Word16, Word32, Word64, Word8)
import           Database.Beam.Backend.SQL (SqlNull)
import           Database.Beam.MySQL.FromField.DecodeError (DecodeError (DecodeError),
                                                            Lenient (IEEEInfinity, IEEENaN, IEEETooBig, IEEETooSmall, SomeStrict, TextCouldNotParse),
                                                            Strict (NotValidJSON, TypeMismatch))
import qualified Database.Beam.MySQL.FromField.Strict as Strict
import           Database.MySQL.Base (MySQLValue (..))
import           Text.Read (readMaybe)
import           Type.Reflection (TyCon, Typeable, typeRep, typeRepTyCon)

class (Strict.FromField a) => FromField (a :: Type) where
  {-# INLINEABLE fromField #-}
  fromField :: MySQLValue -> Either (DecodeError Lenient) a
  fromField v = case Strict.fromField v of
    Left (DecodeError err t) -> Left . DecodeError (SomeStrict err) $ t
    Right res                -> pure res

instance FromField Bool

instance FromField Int8 where
  {-# INLINEABLE fromField #-}
  fromField = tryIntegral

instance FromField Int16 where
  {-# INLINEABLE fromField #-}
  fromField = tryIntegral

instance FromField Int32 where
  {-# INLINEABLE fromField #-}
  fromField = tryIntegral

instance FromField Int64 where
  {-# INLINEABLE fromField #-}
  fromField = tryIntegral

instance FromField Int where
  {-# INLINEABLE fromField #-}
  fromField = tryIntegral

instance FromField Word8 where
  {-# INLINEABLE fromField #-}
  fromField = tryIntegral

instance FromField Word16 where
  {-# INLINEABLE fromField #-}
  fromField = tryIntegral

instance FromField Word32 where
  {-# INLINEABLE fromField #-}
  fromField = tryIntegral

instance FromField Word64 where
  {-# INLINEABLE fromField #-}
  fromField = tryIntegral

instance FromField Word where
  {-# INLINEABLE fromField #-}
  fromField = tryIntegral

instance FromField Scientific

instance FromField Float where
  {-# INLINEABLE fromField #-}
  fromField = tryFloating

instance FromField Double where
  {-# INLINEABLE fromField #-}
  fromField = tryFloating

instance FromField SqlNull

instance FromField Text where
  {-# INLINABLE fromField #-}
  fromField v = case Strict.fromField v of
    Left (DecodeError err t) -> case err of
      TypeMismatch -> case v of
        MySQLInt8 v'    -> pure . pack . show $ v'
        MySQLInt8U v'   -> pure . pack . show $ v'
        MySQLInt16 v'   -> pure . pack . show $ v'
        MySQLInt16U v'  -> pure . pack . show $ v'
        MySQLInt32 v'   -> pure . pack . show $ v'
        MySQLInt32U v'  -> pure . pack . show $ v'
        MySQLInt64 v'   -> pure . pack . show $ v'
        MySQLInt64U v'  -> pure . pack . show $ v'
        MySQLDecimal v' -> pure . pack . show $ v'
        MySQLFloat v'   -> pure . pack . show $ v'
        MySQLDouble v'  -> pure . pack . show $ v'
        _               -> Left . DecodeError (SomeStrict err) $ t
      _ -> Left . DecodeError (SomeStrict err) $ t
    Right res -> pure res

instance FromField ByteString

instance FromField Day

instance FromField TimeOfDay

instance FromField LocalTime

instance (Typeable a, FromJSON a) => FromField (ViaJson a) where
  {-# INLINABLE fromField #-}
  fromField v = case Strict.fromField v of
    Left (DecodeError err t) -> case err of
      TypeMismatch -> case v of
        MySQLBytes v' -> case decodeStrict v' of
          Nothing ->
            Left . DecodeError (SomeStrict NotValidJSON) $ tyCon @a
          Just x  -> pure . ViaJson $ x
        _             -> Left . DecodeError (SomeStrict err) $ t
      _ -> Left . DecodeError (SomeStrict err) $ t
    Right res -> pure res

instance FromField FakeUTC


-- Helpers

tryFloating :: (Strict.FromField a, Read a, RealFloat a, Typeable a) =>
  MySQLValue -> Either (DecodeError Lenient) a
tryFloating v = case Strict.fromField v of
  Left (DecodeError err t) -> case err of
    TypeMismatch -> case v of
      MySQLText v' -> tryIEEEFromText v'
      _            -> Left . DecodeError (SomeStrict err) $ t
    _ -> Left . DecodeError (SomeStrict err) $ t
  Right res -> pure res

tryIEEEFromText :: forall (a :: Type) .
  (Read a, RealFloat a, Typeable a) =>
  Text -> Either (DecodeError Lenient) a
tryIEEEFromText = \case
  "Infinity" -> pure . read $ "Infinity"
  "-Infinity" -> pure . read $ "-Infinity"
  t -> case readMaybe . unpack $ t of
    Nothing -> Left . DecodeError TextCouldNotParse $ tyCon @a
    Just v -> if isInfinite v
                then case signum v of
                  -1 -> Left . DecodeError IEEETooSmall $ tyCon @a
                  _  -> Left . DecodeError IEEETooBig $ tyCon @a
                else pure v

tryIntegral :: (Strict.FromField a, Read a, Typeable a, Integral a, Bounded a) =>
  MySQLValue -> Either (DecodeError Lenient) a
tryIntegral v = case Strict.fromField v of
  Left (DecodeError err t) -> case err of
    TypeMismatch -> case v of
      MySQLText v'   -> tryText v'
      MySQLFloat v'  -> tryIEEE v'
      MySQLDouble v' -> tryIEEE v'
      _              -> Left . DecodeError (SomeStrict err) $ t
    _ -> Left . DecodeError (SomeStrict err) $ t
  Right res -> pure res

tryText :: forall (a :: Type) .
  (Read a, Typeable a) =>
  Text -> Either (DecodeError Lenient) a
tryText t = case readMaybe . unpack $ t of
  Nothing -> Left . DecodeError TextCouldNotParse $ tyCon @a
  Just x  -> pure x

tryIEEE :: forall (b :: Type) (a :: Type) .
  (Integral b, RealFloat a, Bounded b, Typeable b) =>
  a -> Either (DecodeError Lenient) b
tryIEEE v
  | isNaN v = Left . DecodeError IEEENaN $ tyCon @b
  | isInfinite v = Left . DecodeError IEEEInfinity $ tyCon @b
  | v < fromIntegral (minBound @b) =
      Left. DecodeError IEEETooSmall $ tyCon @b
  | v > fromIntegral (maxBound @b) =
      Left . DecodeError IEEETooBig $ tyCon @b
  | otherwise = pure . truncate $ v

tyCon :: forall (a :: Type) . (Typeable a) => TyCon
tyCon = typeRepTyCon $ typeRep @a
