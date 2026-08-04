{-# LANGUAGE DeriveAnyClass #-}

module DB.BadSchemaNullable (BadSchemaNullableT(..)) where

import qualified GHC.Records
import qualified GHC.Records.Extra
import           Data.Int (Int64)
import           Data.Kind (Type)
import           Data.Text (Text)
import           Database.Beam (Beamable, Columnar,
                                Table (PrimaryKey, primaryKey))
import           GHC.Generics (Generic)
import           Prelude hiding (id)

data BadSchemaNullableT (f :: Type -> Type) = BadSchemaNullableT
  { id     :: Columnar f Int64,
    field1 :: Columnar f (Maybe Text),
    field2 :: Columnar f Int64,
    field3 :: Columnar f (Maybe Bool),
    field4 :: Columnar f (Maybe Double)
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

instance Table BadSchemaNullableT where
  data PrimaryKey BadSchemaNullableT (f :: Type -> Type) =
    BadSchemaNullableTPK (Columnar f Int64)
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = BadSchemaNullableTPK . id
