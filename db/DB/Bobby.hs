{-# LANGUAGE DeriveAnyClass #-}

module DB.Bobby (BobbyT(..)) where

import qualified GHC.Records
import qualified GHC.Records.Extra
import           Data.Int (Int64)
import           Data.Kind (Type)
import           Data.Text (Text)
import           Database.Beam (Beamable, Columnar,
                                Table (PrimaryKey, primaryKey))
import           GHC.Generics (Generic)
import           Prelude hiding (id)

data BobbyT (f :: Type -> Type) = BobbyT
  {
    id       :: Columnar f Int64,
    badText  :: Columnar f Text,
    badText2 :: Columnar f Text
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

instance Table BobbyT where
  data PrimaryKey BobbyT (f :: Type -> Type) =
    BobbyTPK (Columnar f Int64)
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = BobbyTPK . id
