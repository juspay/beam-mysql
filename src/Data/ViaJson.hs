{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.ViaJson where

import qualified GHC.Records
import qualified GHC.Records.Extra
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Hashable (Hashable)
import           Data.Kind (Type)
import           Generics.SOP.TH (deriveGeneric)
import           Type.Reflection (Typeable)

-- | A newtype to decode data stores as text, but in the form of JSON.
--
-- While MySQL does support JSON as a first-class data type, it is sometimes
-- easier or more convenient to store the JSON as text. If we know this to be
-- the case, we can use this newtype wrapper to decode database values stored in
-- such a way.
--
-- This is safe even if the value is not actually JSON in the database - you
-- will receive a decoding error in such a situation.
--
-- @since 1.2.1.0
newtype ViaJson (a :: Type) = ViaJson a -- ^ @since 1.2.1.0
  deriving (
            Eq -- ^ @since 1.2.1.0
            , Ord -- ^ @since 1.2.1.0
            , FromJSON -- ^ @since 1.2.1.0
            , ToJSON -- ^ @since 1.2.1.0
            , Hashable -- ^ @since 1.2.1.0
            ) via a
  deriving stock (
                  Show -- ^ @since 1.2.1.0
                  , Typeable -- ^ @since 1.2.1.0
                  )

deriveGeneric ''ViaJson
