{-# LANGUAGE RankNTypes #-}

module Main (main) where

import qualified GHC.Records
import qualified GHC.Records.Extra
import           Control.Exception.Safe (bracket, finally)
import           DB (latin1, testDB)
import           DB.Latin1 (Latin1T (Latin1T))
import qualified DB.Latin1 as Latin1
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import           Data.Char (ord)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeLatin1)
import           Database.Beam.MySQL (MySQL, runBeamMySQL)
import           Database.Beam.Query (QExpr, SqlInsert, SqlSelect, all_, asc_,
                                      default_, delete, insert,
                                      insertExpressions, orderBy_, runDelete,
                                      runInsert, runSelectReturningList, select,
                                      val_, (==.))
import           Database.MySQL.Base (MySQLConn, ciCharset, ciDatabase, ciHost,
                                      ciPort, ciUser, close, connect,
                                      defaultConnectInfoMB4)
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
cleanUp conn = runBeamMySQL conn . runDelete $ delete (latin1 testDB) predicate
  where
    predicate ::
      forall s . (forall s' . Latin1T (QExpr MySQL s')) -> QExpr MySQL s Bool
    predicate row = row ==. row -- Unconditionally true. - Koz

sampleData :: [Text]
sampleData = decodeLatin1 . BS.singleton <$> [0x80 .. 0xff]

mkInsert :: [Text] -> SqlInsert MySQL Latin1T
mkInsert dat = insert (latin1 testDB) (insertExpressions $ fmap go dat)
  where
    go :: forall s . Text -> Latin1T (QExpr MySQL s)
    go d = Latin1T default_ (val_ d)

theQuery :: SqlSelect MySQL Text
theQuery = select $ do
  res <- orderBy_ (asc_ . Latin1.id) . all_ . latin1 $ testDB
  pure . Latin1.dat $ res

runTests :: [Text] -> TestTree
runTests = testGroup "Latin1 handling" . zipWith go sampleData
  where
    go :: Text -> Text -> TestTree
    go expected actual = testCase (testName expected) $ do
      assertEqual "" expected actual

testName :: Text -> String
testName t = "Round trip: '" <> singleByte <> "'"
  where
    singleByte :: String
    singleByte = show . ord . BS8.head . BS8.pack . T.unpack $ t

{-

{-# LANGUAGE DeriveAnyClass #-}

module Main (main) where

import           Control.Exception.Safe (bracket)
import qualified Data.ByteString as BS
import           Data.Foldable (traverse_)
import           Data.Functor.Identity (Identity)
import           Data.Int (Int64)
import           Data.Kind (Type)
import           Data.Text (Text)
import           Data.Text.Encoding (decodeLatin1)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Database.Beam (Beamable, Columnar, Database, DatabaseSettings,
                                Table (PrimaryKey, primaryKey), TableEntity,
                                all_, asc_, defaultDbSettings, insert,
                                insertValues, orderBy_, runInsert,
                                runSelectReturningList, select)
import           Database.Beam.MySQL (MySQL, runBeamMySQL)
import           Database.Beam.Query (QExpr, SqlInsertValues)
import           Database.MySQL.Base (MySQLConn, Query (Query), close, connect,
                                      execute_)
import           Database.MySQL.Temp (MySQLDB, toConnectInfo, withTempDB)
import           GHC.Generics (Generic)
import           Test.Hspec (Spec, describe, hspec, it, shouldBe)

main :: IO ()
main = do
  res <- withTempDB go
  hspec . spec $ res
  where
    go :: MySQLDB -> IO (Vector Text)
    go db = bracket (connect . toConnectInfo $ db)
                    close
                    (\conn -> setUpDB conn >> insertSample conn >> retrieveSample conn)

-- Helpers

spec :: Vector Text -> Spec
spec v = describe "Latin-1-only valid round-tripping" $ do
  it "should round-trip properly" $ do
    v `shouldBe` sampleData

-- Every byte from 0x80 to 0xFF as a singleton string
sampleData :: Vector Text
sampleData = V.fromList . fmap (decodeLatin1 . BS.singleton) $ [0x80 .. 0xFF]

setUpDB :: MySQLConn -> IO ()
setUpDB conn = traverse_ (execute_ conn) [
  "create database test;",
  "use test",
  makeTestTable
  ]
  where
    makeTestTable :: Query
    makeTestTable = Query $
      "create table test_table (" <>
      "id bigint primary key not null, " <>
      "data varchar(255) not null);"

insertSample :: MySQLConn -> IO ()
insertSample conn = runBeamMySQL conn . runInsert . insert (_testTestTable testDB) $ go
  where
    go :: SqlInsertValues MySQL (TestT (QExpr MySQL s))
    go = insertValues . V.toList . V.imap buildTestT $ sampleData
    buildTestT :: Int -> Text -> TestT Identity
    buildTestT i t = TestT (fromIntegral i) t

retrieveSample :: MySQLConn -> IO (Vector Text)
retrieveSample conn =
  fmap V.fromList . runBeamMySQL conn . runSelectReturningList . select $ do
    res <- orderBy_ (asc_ . _testId) . all_ . _testTestTable $ testDB
    pure . _testData $ res

-- Beam boilerplate

data TestT (f :: Type -> Type) = TestT
  {
    _testId   :: Columnar f Int64,
    _testData :: Columnar f Text
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

instance Table TestT where
  data PrimaryKey TestT (f :: Type -> Type) =
    TestTPK (Columnar f Int64)
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = TestTPK . _testId

newtype TestDB (f :: Type -> Type) = TestDB
  {
    _testTestTable :: f (TableEntity TestT)
  }
  deriving stock (Generic)
  deriving anyclass (Database MySQL)

testDB :: DatabaseSettings MySQL TestDB
testDB = defaultDbSettings -}
