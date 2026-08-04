{-# LANGUAGE DeriveAnyClass #-}

module DB.Unicode (UnicodeT(..)) where

import qualified GHC.Records
import qualified GHC.Records.Extra
import           Data.Int (Int64)
import           Data.Kind (Type)
import           Data.Text (Text)
import           Database.Beam (Beamable, Columnar,
                                Table (PrimaryKey, primaryKey))
import           GHC.Generics (Generic)
import           Prelude hiding (id)

data UnicodeT (f :: Type -> Type) = UnicodeT
  { id  :: Columnar f Int64,
    dat :: Columnar f Text
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

instance Table UnicodeT where
  data PrimaryKey UnicodeT (f :: Type -> Type) =
    UnicodeTPK (Columnar f Int64)
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = UnicodeTPK . id
