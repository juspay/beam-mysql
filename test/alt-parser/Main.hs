{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import qualified GHC.Records
import qualified GHC.Records.Extra
import           Control.Concurrent (getNumCapabilities)
import           Control.Exception.Safe (bracket, try)
import           DB (altParser, testDB)
import           DB.AltParser (AltParserT (..))
import           Data.Functor.Identity (Identity)
import qualified Data.HashSet as HS
import           Data.Int (Int64)
import           Database.Beam (SqlSelect, all_, runSelectReturningOne, select)
import           Database.Beam.MySQL (ColumnDecodeError (FoundUnexpectedNull),
                                      MySQL, columnName, demandedType,
                                      runBeamMySQL, sqlType, tablesInvolved)
import           Database.MySQL.Base (MySQLConn, ciCharset, ciDatabase, ciHost,
                                      ciPort, ciUser, close, connect,
                                      defaultConnectInfoMB4)
import           Pool (Pool, create, release, withResource)
import           Test.Tasty (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit (Assertion, assertBool, assertEqual,
                                   assertFailure, testCase)
main :: IO ()
main = bracket (getNumCapabilities >>= create mkConnection)
               (release dropConn)
               (defaultMain . tests)
-- Helpers

mkConnection :: Int64 -> IO MySQLConn
mkConnection _ = do
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

tests :: Pool MySQLConn -> TestTree
tests p = testGroup "AltParser"
  [ testCase "Alt parser error reporting"  . withResource p $ go
  ]
  where
    go :: Int64 -> MySQLConn -> Assertion
    go _ conn = do
      res <- runQueryCatching conn query
      case res of
        Left cde@FoundUnexpectedNull{} -> do
          assertBool "contains table name" (HS.member "alt_parser" . tablesInvolved $ cde)
          assertEqual "indicates correct column" "some_double" . columnName $ cde
          assertEqual "indicates correct demanded type" "Double" . demandedType $ cde
          assertEqual "indicates correct sql type" "MySQL Double" . sqlType $ cde
        Left e -> assertFailure $ show e -- "Did not indicate we got an unexpected NULL."
        Right _  -> assertFailure "Got a result when not expecting any."

runQueryCatching ::
  MySQLConn ->
  SqlSelect MySQL (AltParserT Identity) ->
  IO (Either ColumnDecodeError (Maybe (AltParserT Identity)))
runQueryCatching conn =
 try . runBeamMySQL conn . runSelectReturningOne

query :: SqlSelect MySQL (AltParserT Identity)
query = select . all_ . altParser $ testDB
