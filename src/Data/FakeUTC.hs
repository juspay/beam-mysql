module Data.FakeUTC where

import qualified GHC.Records
import qualified GHC.Records.Extra
import           Data.Time (UTCTime)
import           Type.Reflection (Typeable)

-- | A newtype wrapper to simplify reading and writing UTC times from MySQL
-- databases.
--
-- This is needed because MySQL assumes all times are zoned to whatever time
-- zone the database is set to. As a result, we cannot decode 'UTCTime'
-- directly, as this would require every column decode to check what time zone
-- the database is using.
--
-- If we /know/ for a fact that our database is in UTC, we can use this newtype
-- to decode UTCTime. It is notably unsafe - only use this if you are certain
-- that the time zone in use by the database is indeed UTC.
--
-- @since 1.2.1.0
newtype FakeUTC = FakeUTC UTCTime -- ^ @since 1.2.1.0
  deriving (
            Eq -- ^ @since 1.2.1.0
            , Ord -- ^ @since 1.2.1.0
            )
  deriving stock (
                  Show -- ^ @since 1.2.1.0
                  , Typeable -- ^ @since 1.2.1.0
                  )
