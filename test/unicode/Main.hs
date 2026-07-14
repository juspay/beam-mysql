{-# LANGUAGE RankNTypes #-}

module Main (main) where

import qualified GHC.Records
import qualified GHC.Records.Extra
import           Control.Exception.Safe (bracket, finally)
import           DB (testDB, unicode)
import           DB.Unicode (UnicodeT (UnicodeT))
import qualified DB.Unicode as Unicode
import           Data.Char (GeneralCategory (NotAssigned, PrivateUse),
                            generalCategory)
import           Data.List.Split (chunksOf)
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.Beam.MySQL (MySQL, runBeamMySQL)
import           Database.Beam.Query (QExpr, SqlInsert, SqlSelect, all_, asc_,
                                      default_, delete, insert,
                                      insertExpressions, orderBy_, runDelete,
                                      runInsert, runSelectReturningList, select,
                                      val_, (==.))
import           Database.MySQL.Base (MySQLConn, ciCharset, ciDatabase, ciHost,
                                      ciPort, ciUser, close, connect,
                                      defaultConnectInfoMB4)
import           Fmt ((+|), (|+))
import           Test.Tasty (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit (assertEqual, testCase)

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
    go conn = finally (doTests conn) (cleanUp conn)

doTests :: MySQLConn -> IO ()
doTests conn = do
  runBeamMySQL conn . runInsert . mkInsert $ sampleData
  res <- runBeamMySQL conn . runSelectReturningList $ theQuery
  defaultMain  . runTests $ res

cleanUp :: MySQLConn -> IO ()
cleanUp conn = runBeamMySQL conn . runDelete $ delete (unicode testDB) predicate
  where
    predicate ::
      forall s . (forall s' . UnicodeT (QExpr MySQL s')) -> QExpr MySQL s Bool
    predicate row = row ==. row -- Unconditionally true. - Koz

mkInsert :: [Text] -> SqlInsert MySQL UnicodeT
mkInsert dat = insert (unicode testDB) (insertExpressions $ fmap go dat)
  where
    go :: forall s . Text -> UnicodeT (QExpr MySQL s)
    go d = UnicodeT default_ (val_ d)

sampleData :: [Text]
sampleData = fmap T.pack . take 20 . chunksOf 8 . filter go $ [minBound .. maxBound]
  where
    go :: Char -> Bool
    go c = case generalCategory c of
      NotAssigned -> False
      PrivateUse  -> False
      _           -> True

theQuery :: SqlSelect MySQL Text
theQuery = select $ do
  res <- orderBy_ (asc_ . Unicode.id) . all_ . unicode $ testDB
  pure . Unicode.dat $ res

runTests :: [Text] -> TestTree
runTests = testGroup "Unicode handling" . zipWith go sampleData
  where
    go :: Text -> Text -> TestTree
    go expected actual = testCase (testName expected) $ do
      assertEqual "" expected actual
    testName :: Text -> String
    testName t = let t' = fmap fromEnum . T.unpack $ t in
      "Round trip: \'" +| t' |+ "\'"
