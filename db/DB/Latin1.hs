{-# LANGUAGE DeriveAnyClass #-}

module DB.Latin1 (Latin1T(..)) where

import qualified GHC.Records
import qualified GHC.Records.Extra
import           Data.Int (Int64)
import           Data.Kind (Type)
import           Data.Text (Text)
import           Database.Beam (Beamable, Columnar,
                                Table (PrimaryKey, primaryKey))
import           GHC.Generics (Generic)
import           Prelude hiding (id)

data Latin1T (f :: Type -> Type) = Latin1T
  { id  :: Columnar f Int64,
    dat :: Columnar f Text
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

instance Table Latin1T where
  data PrimaryKey Latin1T (f :: Type -> Type) =
    Latin1TPK (Columnar f Int64)
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = Latin1TPK . id
