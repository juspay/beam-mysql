module Main (main) where

import qualified GHC.Records
import qualified GHC.Records.Extra
import           Control.Exception.Safe (bracket)
import           DB (lenient, testDB)
import           DB.Lenient (LenientT)
import qualified DB.Lenient as Lenient
import           Data.AEq ((~==))
import           Data.Functor.Identity (Identity)
import qualified Data.Vector as V
import           Database.Beam.MySQL (ViaJson (ViaJson), runBeamMySQL)
import           Database.Beam.Query (all_, runSelectReturningOne, select)
import           Database.MySQL.Base (MySQLConn, ciCharset, ciDatabase, ciHost,
                                      ciPort, ciUser, close, connect,
                                      defaultConnectInfoMB4)
import           Test.Tasty (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit (Assertion, assertBool, assertEqual,
                                   assertFailure, testCase)

main :: IO ()
main = bracket mkConn close go
  where
    mkConn :: IO MySQLConn
    mkConn = do
      let connInfo = defaultConnectInfoMB4 {
                        ciHost = "localhost",
                        ciPort = 3306,
                        ciDatabase = "test",
                        ciUser = "root",
                        ciCharset = 8
                        }
      connect connInfo
    go :: MySQLConn -> IO ()
    go conn = do
      res <- runBeamMySQL conn .
              runSelectReturningOne .
              select .
              all_ .
              lenient $ testDB
      defaultMain . runTests $ res

runTests :: Maybe (LenientT Identity) -> TestTree
runTests res = testGroup "Lenient decoding" [
  testCase "Row decode" go
  ]
  where
    go :: Assertion
    go = case res of
      Nothing  -> assertFailure "Did not receive a result"
      Just row -> do
        assertEqual "Int8 from VARCHAR" 10 . Lenient.int8Varchar $ row
        assertEqual "Int16 from VARCHAR" 256 . Lenient.int16Varchar $ row
        assertEqual "Int32 from VARCHAR" 65666 . Lenient.int32Varchar $ row
        assertEqual "Int64 from VARCHAR" 5000000000 . Lenient.int64Varchar $ row
        assertBool "Float from VARCHAR" $ 10.05 ~== Lenient.floatVarchar row
        assertBool "Double from VARCHAR" $ 10.0500505005 ~== Lenient.doubleVarchar row
        assertEqual "Text from TINYINT" "10" . Lenient.textTinyint $ row
        assertEqual "Text from SMALLINT" "256" . Lenient.textSmallint $ row
        assertEqual "Text from INT" "65666" . Lenient.textInt $ row
        assertEqual "Text from BIGINT" "5000000000" . Lenient.textBigint $ row
        assertEqual "Text from FLOAT" "10.05" . Lenient.textFloat $ row
        assertEqual "Int8 from FLOAT" 10 . Lenient.int8Float $ row
        assertEqual "Int16 from FLOAT" 10 . Lenient.int16Float $ row
        assertEqual "Int32 from FLOAT" 10 . Lenient.int32Float $ row
        assertEqual "Int64 from FLOAT" 10 . Lenient.int64Float $ row
        assertEqual "Text from DOUBLE" "10.0500505005" . Lenient.textDouble $ row
        assertEqual "Int8 from DOUBLE" 10 . Lenient.int8Double $ row
        assertEqual "Int16 from DOUBLE" 10 . Lenient.int16Double $ row
        assertEqual "Int32 from DOUBLE" 10 . Lenient.int32Double $ row
        assertEqual "Int64 from DOUBLE" 10 . Lenient.int64Double $ row
        assertEqual "ViaJson from BINARY" (ViaJson . V.fromList $ [1, 2, 3, 4]) .
          Lenient.viajsonBinary $ row
