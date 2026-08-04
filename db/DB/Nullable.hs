{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE StandaloneDeriving #-}

module DB.Nullable (NullableT(..)) where

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

data NullableT (f :: Type -> Type) = NullableT
  { id  :: Columnar f Int64,
    dat :: Columnar f Text
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

deriving stock instance Show (NullableT Identity)

instance Table NullableT where
  data PrimaryKey NullableT (f :: Type -> Type) =
    NullableTPK (Columnar f Int64)
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = NullableTPK . id
