{-# LANGUAGE RankNTypes #-}

module Main (main) where

import qualified GHC.Records
import qualified GHC.Records.Extra
import           Control.Concurrent (getNumCapabilities)
import qualified Control.Exception.Lifted as LB
import           Control.Exception.Safe (IOException, bracket, finally)
import           Control.Monad.IO.Class (liftIO)
import           DB (noPk, pkAi, pkAi2, pkNoAi, testDB)
import           DB.PrimaryKey.AutoInc (AutoIncT (AutoIncT))
import qualified DB.PrimaryKey.AutoInc as AutoInc
import           DB.PrimaryKey.NoAutoInc (NoAutoIncT (NoAutoIncT))
import qualified DB.PrimaryKey.NoAutoInc as NoAutoInc
import           DB.PrimaryKey.None (NoneT (NoneT))
import qualified DB.PrimaryKey.None as None
import           Data.Functor.Identity (Identity)
import           Data.Int (Int64)
import           Database.Beam.MySQL (MySQL,
                                      MySQLStatementError (OperationNotSupported),
                                      runBeamMySQL, runInsertRowReturning)
import           Database.Beam.Query (QExpr, SqlInsertValues, default_, delete,
                                      insert, insertExpressions, insertValues,
                                      runDelete, val_, (==.))
import           Database.MySQL.Base (MySQLConn, ciCharset, ciDatabase, ciHost,
                                      ciPort, ciUser, close, connect,
                                      defaultConnectInfoMB4)
import           Hedgehog (Gen, PropertyT, failure, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Pool (Pool, create, release, withResource, withResource')
import           Test.Tasty (TestTree, defaultMain, localOption, testGroup)
import           Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import           Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit),
                                      testProperty)

main :: IO ()
main = bracket (getNumCapabilities >>= create mkConn)
               (release dropConn)
               (defaultMain . runTests)
  where
    mkConn :: Int64 -> IO MySQLConn
    mkConn _ = do
      let connInfo = defaultConnectInfoMB4 {
                        ciHost = "localhost",
                        ciPort = 3306,
                        ciDatabase = "test",
                        ciUser = "root",
                        ciCharset = 8
                        }
      connect connInfo
    dropConn :: Int64 -> MySQLConn -> IO ()
    dropConn _ = close

runTests :: Pool MySQLConn -> TestTree
runTests p = localOption (HedgehogTestLimit . Just $ 1000) .
  testGroup "runInsertRowReturning" $ [
    testCase "Autoincrement primary key table inserts" .
      withResource p $ autoIncProp,
    testProperty "Autoincrement primary key table inserts with known value" .
      property .
      withResource' p $ autoIncProp',
    testProperty "Ordinary table inserts" .
      property .
      withResource' p $ noAutoIncProp,
    testProperty "Erroring on inserts into tables without primary keys" .
      property .
      withResource' p $ noPkProp
    ]

autoIncProp :: Int64 -> MySQLConn -> IO ()
autoIncProp _ conn = finally doTest cleanup
  where
    doTest :: IO ()
    doTest = do
      res <- runBeamMySQL conn .
              runInsertRowReturning .
              insert (pkAi testDB) $
              go
      case res of
        Nothing               -> assertFailure "Should have produced result"
        Just (AutoIncT _ dat) -> assertEqual "" "foo" dat
    cleanup :: IO ()
    cleanup = runBeamMySQL conn . runDelete $ delete (pkAi testDB) predicate
    predicate ::
      forall s . (forall s' . AutoIncT (QExpr MySQL s')) -> QExpr MySQL s Bool
    predicate row = row ==. row
    go :: forall s . SqlInsertValues MySQL (AutoIncT (QExpr MySQL s))
    go = insertExpressions [AutoIncT default_ (val_ "foo")]

autoIncProp' :: Int64 -> MySQLConn -> PropertyT IO ()
autoIncProp' i conn = do
  row <- forAll genRow
  LB.finally (roundtrip row) (liftIO cleanup)
  where
    genRow :: Gen (AutoIncT Identity)
    genRow = AutoIncT i <$> Gen.text (Range.linear 0 10) Gen.unicode
    roundtrip :: AutoIncT Identity -> PropertyT IO ()
    roundtrip row = do
      row' <- LB.handle @_ @IOException (const failure) (liftIO . insertTheRow $ row)
      Just row === row'
    insertTheRow :: AutoIncT Identity -> IO (Maybe (AutoIncT Identity))
    insertTheRow row =
      runBeamMySQL conn .
      runInsertRowReturning .
      insert (pkAi2 testDB) .
      insertValues $ [row]
    cleanup :: IO ()
    cleanup =
      runBeamMySQL conn . runDelete $ delete (pkAi2 testDB) predicate
    predicate ::
      forall s . (forall s' . AutoIncT (QExpr MySQL s')) -> QExpr MySQL s Bool
    predicate row = AutoInc.id row ==. val_ i

noAutoIncProp :: Int64 -> MySQLConn -> PropertyT IO ()
noAutoIncProp i conn = do
  row <- forAll genRow
  LB.finally (roundtrip row) (liftIO cleanup)
  where
    genRow :: Gen (NoAutoIncT Identity)
    genRow = NoAutoIncT i <$> Gen.text (Range.linear 0 10) Gen.unicode
    roundtrip :: NoAutoIncT Identity -> PropertyT IO ()
    roundtrip row = do
      row' <- LB.handle @_ @IOException (const failure) (liftIO . insertTheRow $ row)
      Just row === row'
    insertTheRow :: NoAutoIncT Identity -> IO (Maybe (NoAutoIncT Identity))
    insertTheRow row =
      runBeamMySQL conn .
      runInsertRowReturning .
      insert (pkNoAi testDB) .
      insertValues $ [row]
    cleanup :: IO ()
    cleanup =
      runBeamMySQL conn . runDelete $ delete (pkNoAi testDB) predicate
    predicate ::
      forall s . (forall s' . NoAutoIncT (QExpr MySQL s')) -> QExpr MySQL s Bool
    predicate row = NoAutoInc.id row ==. val_ i

noPkProp :: Int64 -> MySQLConn -> PropertyT IO ()
noPkProp i conn = do
  row <- forAll genRow
  LB.finally (go row) (liftIO cleanup)
  where
    genRow :: Gen (NoneT Identity)
    genRow = NoneT i <$> Gen.text (Range.linear 0 10) Gen.unicode
    go :: NoneT Identity -> PropertyT IO ()
    go row = do
      res <- LB.catch (liftIO . tryTheInsert $ row) (pure . Just)
      case res of
        Just (OperationNotSupported op c _) -> do
          "Insert row returning without primary key" === op
          "no_pk" === c
        _                                   -> failure
    cleanup :: IO ()
    cleanup =
      runBeamMySQL conn . runDelete $ delete (noPk testDB) predicate
    predicate ::
      forall s . (forall s' . NoneT (QExpr MySQL s')) -> QExpr MySQL s Bool
    predicate row = None.id row ==. val_ i
    tryTheInsert :: NoneT Identity -> IO (Maybe MySQLStatementError)
    tryTheInsert row = do
      _ <- runBeamMySQL conn .
            runInsertRowReturning .
            insert (noPk testDB) .
            insertValues $ [row]
      pure Nothing
