{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

module DB.AltParser (AltParserT(..)) where

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


data AltParserT (f :: Type -> Type) = EnumType
  { id         :: Columnar f Int64
  , someText   :: Columnar f (Maybe Text)
  , someInt    :: Columnar f Int
  , someDouble :: Columnar f Double
  , dat        :: Columnar f (Maybe Int)
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

deriving stock instance Show (AltParserT Identity)

instance Table AltParserT where
  data PrimaryKey AltParserT (f :: Type -> Type) =
    AltParserTPK (Columnar f Int64)
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = AltParserTPK . id
