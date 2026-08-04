{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pool (
  Pool,
  create, release,
  withResource,
  withResource'
  ) where

import qualified GHC.Records
import qualified GHC.Records.Extra
import           Control.Concurrent.BoundedChan (BoundedChan, newBoundedChan,
                                                 readChan, writeChan)
import qualified Control.Exception.Lifted as LB
import           Control.Exception.Safe (MonadMask, bracket)
import           Control.Monad.Base (liftBase)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Foldable (traverse_)
import           Data.Int (Int64)
import           Data.Kind (Type)

newtype Pool (a :: Type) = Pool (Int, BoundedChan (Int64, a))

create :: forall (a :: Type) . (Int64 -> IO a) -> Int -> IO (Pool a)
create f n
  | n <= 0 = Pool . (0,) <$> newBoundedChan 0
  | otherwise = do
      let n' = fromIntegral n
      chan <- newBoundedChan n'
      traverse_ (go chan) [1 .. fromIntegral n']
      pure . Pool . (n',) $ chan
    where
      go :: BoundedChan (Int64, a) -> Int64 -> IO ()
      go chan i = f i >>= (writeChan chan . (i,))

release :: (Int64 -> a -> IO ()) -> Pool a -> IO ()
release f (Pool (len, chan)) = go 0
  where
    go :: Int -> IO ()
    go acc
      | acc == len = pure ()
      | otherwise = do
          (i, x) <- readChan chan
          f i x
          go (acc + 1)

withResource :: (MonadMask m, MonadIO m) =>
  Pool a -> (Int64 -> a -> m b) -> m b
withResource (Pool (_, chan)) cb =
  bracket (liftIO . readChan $ chan)
          (liftIO . writeChan chan)
          (uncurry cb)

-- Needed for use with Hedgehog, because PropertyT doesn't have a MonadMask
-- instance. - Koz
withResource' :: (MonadBaseControl IO m) =>
  Pool a -> (Int64 -> a -> m b) -> m b
withResource' (Pool (_, chan)) cb =
  LB.bracket (liftBase . readChan $ chan)
             (liftBase . writeChan chan)
             (uncurry cb)
