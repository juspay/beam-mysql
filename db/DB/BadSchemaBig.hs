{-# LANGUAGE DeriveAnyClass #-}

module DB.BadSchemaBig (BadSchemaBigT(..)) where

import qualified GHC.Records
import qualified GHC.Records.Extra
import           Data.Int (Int64)
import           Data.Kind (Type)
import           Data.Text (Text)
import           Database.Beam (Beamable, Columnar,
                                Table (PrimaryKey, primaryKey))
import           GHC.Generics (Generic)
import           Prelude hiding (id)

data BadSchemaBigT (f :: Type -> Type) = BadSchemaBigT
  { id     :: Columnar f Int64,
    field1 :: Columnar f Bool,
    field2 :: Columnar f Int64,
    field3 :: Columnar f (Maybe Text),
    field4 :: Columnar f (Maybe Int64)
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

instance Table BadSchemaBigT where
  data PrimaryKey BadSchemaBigT (f :: Type -> Type) =
    BadSchemaBigTPK (Columnar f Int64)
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = BadSchemaBigTPK . id
