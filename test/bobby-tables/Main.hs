{-# LANGUAGE RankNTypes #-}

module Main (main) where

import qualified GHC.Records
import qualified GHC.Records.Extra
import           Control.Concurrent (getNumCapabilities)
import           Control.Exception.Safe (IOException, bracket, finally, handle)
import           DB (bobby, testDB)
import           DB.Bobby (BobbyT (BobbyT))
import qualified DB.Bobby as Bobby
import           Data.Int (Int64)
import           Data.Text (Text)
import           Database.Beam.MySQL (MySQL, runBeamMySQL,
                                      runInsertRowReturning)
import           Database.Beam.Query (QAssignment, QBaseScope, QExpr, QField,
                                      QGenExpr, QValueContext, SqlInsert,
                                      SqlSelect, all_, delete, guard_, insert,
                                      insertValues, runDelete, runInsert,
                                      runSelectReturningOne, runUpdate, save,
                                      select, update, val_, (<-.), (==.))
import           Database.MySQL.Base (MySQLConn, ciCharset, ciDatabase, ciHost,
                                      ciPort, ciUser, close, connect,
                                      defaultConnectInfoMB4)
import           Pool (Pool, create, release, withResource)
import           Test.Tasty (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit (Assertion, assertEqual, assertFailure,
                                   testCase)

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
runTests p = testGroup "Bobby-Tables-style injection" [
  testCase "select . filter_" . withResource p $ selectFilterTest,
  testCase "insert" . withResource p $ insertTest,
  testCase "update" . withResource p $ updateTest,
  testCase "save" . withResource p $ saveTest,
  testCase "insertReturning" . withResource p $ insertReturningTest
  ]

selectFilterTest :: Int64 -> MySQLConn -> Assertion
selectFilterTest _ conn = do
  res <- handle @_ @IOException (const (assertFailure "query threw"))
                                (doTheSelect conn)
  assertEqual "" (Just bobbyTables) res

insertTest :: Int64 -> MySQLConn -> Assertion
insertTest _ conn = finally go removeEntry
  where
    go :: Assertion
    go = do
      handle @_ @IOException (const (assertFailure "insert threw")) doTheInsert
      res <- handle @_ @IOException (const (assertFailure "query threw"))
                                    (doTheSelect conn)
      assertEqual "" (Just bobbyTables) res
    removeEntry :: IO ()
    removeEntry = runBeamMySQL conn . runDelete $ delete (bobby testDB) predicate
    predicate ::
      forall s . (forall s' . BobbyT (QExpr MySQL s')) -> QExpr MySQL s Bool
    predicate row = Bobby.id row ==. val_ 10
    doTheInsert :: IO ()
    doTheInsert = runBeamMySQL conn . runInsert $ theInsert
    theInsert :: SqlInsert MySQL BobbyT
    theInsert = insert (bobby testDB) . insertValues $ [BobbyT 10 "hello" bobbyTables]

updateTest :: Int64 -> MySQLConn -> Assertion
updateTest _ conn = finally go restore
  where
    go :: Assertion
    go = do
      handle @_ @IOException (const (assertFailure "update threw")) doTheUpdate
      res <- handle @_ @IOException (const (assertFailure "query threw"))
                                    (doTheSelect conn)
      assertEqual "" (Just bobbyTables) res
    restore :: IO ()
    restore =
      runBeamMySQL conn . runUpdate $ update (bobby testDB) restoreMod predicate
    restoreMod :: forall s . BobbyT (QField s) -> QAssignment MySQL s
    restoreMod row = Bobby.badText row <-. val_ "foo\'"
    predicate :: forall s . BobbyT (QExpr MySQL s) -> QExpr MySQL s Bool
    predicate row = Bobby.badText2 row ==. val_ bobbyTables
    doTheUpdate :: IO ()
    doTheUpdate =
      runBeamMySQL conn . runUpdate $ update (bobby testDB) updateMod predicate
    updateMod :: forall s . BobbyT (QField s) -> QAssignment MySQL s
    updateMod row = Bobby.badText row <-. val_ bobbyTables

saveTest :: Int64 -> MySQLConn -> Assertion
saveTest _ conn = finally go restore
  where
    go :: Assertion
    go = do
      handle @_ @IOException (const (assertFailure "save threw")) doTheSave
      res <- handle @_ @IOException (const (assertFailure "query threw"))
                                    (doTheSelect conn)
      assertEqual "" (Just bobbyTables) res
    restore :: IO ()
    restore =
      runBeamMySQL conn . runUpdate . save (bobby testDB) $ BobbyT 1 "foo\'" bobbyTables
    doTheSave :: IO ()
    doTheSave =
      runBeamMySQL conn . runUpdate . save (bobby testDB) $ BobbyT 1 "hello" bobbyTables

insertReturningTest :: Int64 -> MySQLConn -> Assertion
insertReturningTest _ conn = finally go restore
  where
    go :: Assertion
    go = do
      res <- handle @_ @IOException (const (assertFailure "insertReturning threw"))
                                    doTheInsertReturning
      assertEqual "" (Just bobbyTables) res
    restore :: IO ()
    restore = runBeamMySQL conn . runDelete $ delete (bobby testDB) predicate
    predicate ::
      forall s . (forall s' . BobbyT (QExpr MySQL s')) -> QExpr MySQL s Bool
    predicate row = Bobby.id row ==. val_ 2
    doTheInsertReturning :: IO (Maybe Text)
    doTheInsertReturning = do
      res <- runBeamMySQL conn . runInsertRowReturning $ theInsert
      pure (Bobby.badText2 <$> res)
    theInsert :: SqlInsert MySQL BobbyT
    theInsert = insert (bobby testDB) . insertValues $ [BobbyT 2 "hello" bobbyTables]

bobbyTables :: Text
bobbyTables = "\'; DROP TABLE students; --"

doTheSelect :: MySQLConn -> IO (Maybe Text)
doTheSelect conn = runBeamMySQL conn . runSelectReturningOne $ query
  where
    query :: SqlSelect MySQL Text
    query = select $ do
      res <- all_ . bobby $ testDB
      guard_ . predicate $ res
      pure . Bobby.badText2 $ res
    predicate ::
      BobbyT (QGenExpr QValueContext MySQL QBaseScope) ->
      QGenExpr QValueContext MySQL QBaseScope Bool
    predicate row = Bobby.badText2 row ==. val_ bobbyTables
