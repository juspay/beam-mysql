{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE StandaloneDeriving #-}

module DB.NullableMaybe (NullableMaybeT(..)) where

import qualified GHC.Records
import qualified GHC.Records.Extra
import           Data.Functor.Identity (Identity)
import           Data.Int (Int64)
import           Data.Kind (Type)
import           Data.Text (Text)
import           Database.Beam (Beamable, Columnar,
                                Table (PrimaryKey, primaryKey))
import           GHC.Generics (Generic)
import           Prelude hiding (id)

data NullableMaybeT (f :: Type -> Type) = NullableMaybeT
  { id  :: Columnar f Int64,
    dat :: Columnar f (Maybe Text)
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

deriving stock instance Show (NullableMaybeT Identity)

instance Table NullableMaybeT where
  data PrimaryKey NullableMaybeT (f :: Type -> Type) =
    NullableMaybeTPK (Columnar f Int64)
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = NullableMaybeTPK . id
