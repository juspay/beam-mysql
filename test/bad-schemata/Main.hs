module Main (main) where

import qualified GHC.Records
import qualified GHC.Records.Extra
import           Control.Concurrent (getNumCapabilities)
import           Control.Exception.Safe (bracket, try)
import           DB (badSchema, badSchemaBig, badSchemaNullable, testDB)
import           DB.BadSchema (BadSchemaT)
import           DB.BadSchemaBig (BadSchemaBigT)
import           DB.BadSchemaNullable (BadSchemaNullableT)
import           Data.Functor.Identity (Identity)
import qualified Data.HashSet as HS
import           Data.Int (Int64)
import           Database.Beam.MySQL (ColumnDecodeError (Can'tDecodeIntoDemanded),
                                      columnName, demandedType, runBeamMySQL,
                                      sqlType, tablesInvolved, value)
import           Database.Beam.Query (all_, runSelectReturningOne, select)
import           Database.MySQL.Base (MySQLConn, ciCharset, ciDatabase, ciHost,
                                      ciPort, ciUser, close, connect,
                                      defaultConnectInfoMB4)
import           Pool (Pool, create, release, withResource)
import           Test.Tasty (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit (assertBool, assertEqual, assertFailure,
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
runTests p = testGroup "Bad schemata" [
  testCase "Errors on mistyped fields (simple)" .
    withResource p $ simpleTests,
  testCase "Errors on mistyped fields (complex)" .
    withResource p $ complexTests,
  testCase "Errors on mistyped fields (nullable)" .
    withResource p $ nullableTests
  ]
  where
    simpleTests :: Int64 -> MySQLConn -> IO ()
    simpleTests _ conn = do
      res <- try . runSimpleQuery $ conn
      case res of
        Right _                        ->
          assertFailure "Got a result, but was meant to throw."
        Left err@Can'tDecodeIntoDemanded{} -> do
          assertBool "Contains table name" . HS.member "bad_schema" . tablesInvolved $ err
          assertEqual "Indicates column" "data" . columnName $ err
          assertEqual "States expected type" "Text" . demandedType $ err
          assertEqual "States SQL type" "MySQL LongLong" . sqlType $ err
          assertEqual "Reports value found" "MySQLInt64 2" . value $ err
        _                              ->
          assertFailure "Wrong type of error indicated."
    complexTests :: Int64 -> MySQLConn -> IO ()
    complexTests _ conn = do
      res <- try . runComplexQuery $ conn
      case res of
        Right _                        ->
          assertFailure "Got a result, but was meant to throw."
        Left err@Can'tDecodeIntoDemanded{} -> do
          assertBool "Contains table name" . HS.member "bad_schema_big" . tablesInvolved $ err
          assertEqual "Indicates column" "field_2" . columnName $ err
          assertEqual "States expected type" "Int64" . demandedType $ err
          assertEqual "States SQL type" "MySQL VarString" . sqlType $ err
          assertEqual "Reports value found" "MySQLText \"bar\"" . value $ err
        _                              ->
          assertFailure "Wrong type of error indicated."
    nullableTests :: Int64 -> MySQLConn -> IO ()
    nullableTests _ conn = do
      res <- try . runNullableQuery $ conn
      case res of
        Right _ ->
          assertFailure "Got a result, but was meant to throw."
        Left err@Can'tDecodeIntoDemanded{} -> do
          assertBool "Contains table name" . HS.member "bad_schema_nullable" . tablesInvolved $ err
          assertEqual "Indicates column" "field_4" . columnName $ err
          assertEqual "States expected type" "Double" . demandedType $ err
          assertEqual "States SQL type" "MySQL Long" . sqlType $ err
          assertEqual "Reports value found" "MySQLInt32 15" . value $ err
        _ ->
          assertFailure "Wrong type of error indicated."

runSimpleQuery ::
  MySQLConn -> IO (Maybe (BadSchemaT Identity))
runSimpleQuery conn =
  runBeamMySQL conn .
  runSelectReturningOne .
  select .
  all_ .
  badSchema $ testDB

runComplexQuery ::
  MySQLConn -> IO (Maybe (BadSchemaBigT Identity))
runComplexQuery conn =
  runBeamMySQL conn .
  runSelectReturningOne .
  select .
  all_ .
  badSchemaBig $ testDB

runNullableQuery ::
  MySQLConn -> IO (Maybe (BadSchemaNullableT Identity))
runNullableQuery conn =
  runBeamMySQL conn .
  runSelectReturningOne .
  select .
  all_ .
  badSchemaNullable $ testDB
