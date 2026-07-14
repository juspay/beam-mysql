{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE StandaloneDeriving #-}

module DB.PrimaryKey.AutoInc (AutoIncT(..)) where

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

data AutoIncT (f :: Type -> Type) = AutoIncT
  { id  :: Columnar f Int64,
    dat :: Columnar f Text
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

instance Table AutoIncT where
  data PrimaryKey AutoIncT (f :: Type -> Type) =
    AutoIncTPK (Columnar f Int64)
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = AutoIncTPK . id

deriving stock instance Eq (AutoIncT Identity)

deriving stock instance Show (AutoIncT Identity)
