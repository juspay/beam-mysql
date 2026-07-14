{-# LANGUAGE DeriveAnyClass #-}

module DB.Lenient (LenientT(..)) where

import qualified GHC.Records
import qualified GHC.Records.Extra
import           Data.Int (Int16, Int32, Int64, Int8)
import           Data.Kind (Type)
import           Data.Text (Text)
import           Data.Vector (Vector)
import           Database.Beam (Beamable, Columnar,
                                Table (PrimaryKey, primaryKey))
import           Database.Beam.MySQL (ViaJson)
import           GHC.Generics (Generic)
import           Prelude hiding (id)

data LenientT (f :: Type -> Type) = LenientT
  {
    id            :: Columnar f Int64,
    int8Varchar   :: Columnar f Int8,
    int16Varchar  :: Columnar f Int16,
    int32Varchar  :: Columnar f Int32,
    int64Varchar  :: Columnar f Int64,
    floatVarchar  :: Columnar f Float,
    doubleVarchar :: Columnar f Double,
    textTinyint   :: Columnar f Text,
    textSmallint  :: Columnar f Text,
    textInt       :: Columnar f Text,
    textBigint    :: Columnar f Text,
    textFloat     :: Columnar f Text,
    int8Float     :: Columnar f Int8,
    int16Float    :: Columnar f Int16,
    int32Float    :: Columnar f Int32,
    int64Float    :: Columnar f Int64,
    textDouble    :: Columnar f Text,
    int8Double    :: Columnar f Int8,
    int16Double   :: Columnar f Int16,
    int32Double   :: Columnar f Int32,
    int64Double   :: Columnar f Int64,
    viajsonBinary :: Columnar f (ViaJson (Vector Int))
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

instance Table LenientT where
  data PrimaryKey LenientT (f :: Type -> Type) =
    LenientTPK (Columnar f Int64)
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = LenientTPK . id

