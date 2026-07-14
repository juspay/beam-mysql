module Database.Beam.MySQL.FromField.DecodeError where

import qualified GHC.Records
import qualified GHC.Records.Extra
import           Data.Kind (Type)
import           Type.Reflection (TyCon)

data Strict =
  UnexpectedNull |
  TypeMismatch |
  Won'tFit |
  NotValidJSON
  deriving stock (Eq, Show)

data Lenient =
  SomeStrict !Strict |
  IEEENaN |
  IEEEInfinity |
  IEEETooSmall |
  IEEETooBig |
  TextCouldNotParse
  deriving stock (Eq, Show)

data DecodeError (e :: Type) =
  DecodeError !e {-# UNPACK #-} !TyCon
  deriving stock (Eq, Show)

