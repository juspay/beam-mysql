{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE StandaloneDeriving #-}

module DB.ViaJSON (ViaJSONT(..)) where

import qualified GHC.Records
import qualified GHC.Records.Extra
import           Data.Aeson (Value)
import           Data.Functor.Identity (Identity)
import           Data.Int (Int64)
import           Data.Kind (Type)
import           Database.Beam (Beamable, Columnar,
                                Table (PrimaryKey, primaryKey))
import           Database.Beam.MySQL (ViaJson)
import           GHC.Generics (Generic)
import           Prelude hiding (id)

data ViaJSONT (f :: Type -> Type) = ViaJSONT
  { id         :: Columnar f Int64,
    fromBool   :: Columnar f (ViaJson Value),
    fromDouble :: Columnar f (ViaJson Value),
    fromString :: Columnar f (ViaJson Value),
    fromArray  :: Columnar f (ViaJson Value),
    fromObject :: Columnar f (ViaJson Value)
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

instance Table ViaJSONT where
  data PrimaryKey ViaJSONT (f :: Type -> Type) =
    ViaJSONTPK (Columnar f Int64)
      deriving stock (Generic)
      deriving anyclass (Beamable)
  primaryKey = ViaJSONTPK . id

deriving stock instance Eq (ViaJSONT Identity)

deriving stock instance Show (ViaJSONT Identity)
