{-# LANGUAGE RankNTypes #-}

module Main (main) where

import qualified GHC.Records
import qualified GHC.Records.Extra
import           Control.Concurrent (getNumCapabilities)
import qualified Control.Exception.Lifted as LB
import           Control.Exception.Safe (IOException, bracket)
import           Control.Monad.IO.Class (liftIO)
import           DB (testDB, viaJson)
import           DB.ViaJSON (ViaJSONT (ViaJSONT))
import qualified DB.ViaJSON as ViaJSON
import           Data.Aeson (Value (Array, Bool, Number, Object, String))
import           Data.Functor.Identity (Identity)
import qualified Data.HashMap.Strict as HM
import           Data.Int (Int64)
import           Data.Scientific (fromFloatDigits)
import           Data.Text (Text)
import qualified Data.Vector as V
import           Database.Beam (primaryKey)
import           Database.Beam.MySQL (MySQL, ViaJson (ViaJson), runBeamMySQL)
import           Database.Beam.Query (QExpr, delete, insert, insertValues,
                                      lookup_, runDelete, runInsert,
                                      runSelectReturningOne, val_, (==.))
import           Database.MySQL.Base (MySQLConn, ciCharset, ciDatabase, ciHost,
                                      ciPort, ciUser, close, connect,
                                      defaultConnectInfoMB4)
import           Hedgehog (Gen, PropertyT, failure, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Pool (Pool, create, release, withResource')
import           Test.Tasty (TestTree, defaultMain, localOption, testGroup)
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
  testGroup "ViaJson wrapper" $ [
    testProperty "roundtrip" . property . withResource' p $ go
    ]
  where
    go :: Int64 -> MySQLConn -> PropertyT IO ()
    go i conn = do
      row <- forAll . genRow $ i
      LB.finally (roundtrip conn row) (removeDB i conn)

removeDB :: Int64 -> MySQLConn -> PropertyT IO ()
removeDB i conn =
  liftIO . runBeamMySQL conn . runDelete $ delete (viaJson testDB) go
  where
    go :: forall s . (forall s' . ViaJSONT (QExpr MySQL s')) -> QExpr MySQL s Bool
    go expr = val_ i ==. ViaJSON.id expr

roundtrip :: MySQLConn -> ViaJSONT Identity -> PropertyT IO ()
roundtrip conn row = do
  LB.handle @_ @IOException (const failure) (liftIO insertTheRow)
  row' <- LB.handle @_ @IOException (const failure) (liftIO selectTheRow)
  Just row === row'
  where
    insertTheRow :: IO ()
    insertTheRow =
      runBeamMySQL conn .
      runInsert .
      insert (viaJson testDB) .
      insertValues $ [row]
    selectTheRow :: IO (Maybe (ViaJSONT Identity))
    selectTheRow =
      runBeamMySQL conn .
      runSelectReturningOne .
      lookup_ (viaJson testDB) .
      primaryKey $ row

-- Generators

genRow :: Int64 -> Gen (ViaJSONT Identity)
genRow pk =
  ViaJSONT pk <$> (ViaJson <$> genJBool) <*>
                  (ViaJson <$> genJNum) <*>
                  (ViaJson <$> genJString) <*>
                  (ViaJson <$> genJArray) <*>
                  (ViaJson <$> genJObject)

genJBool :: Gen Value
genJBool = Bool <$> Gen.bool

genJNum :: Gen Value
genJNum =
  Number . fromFloatDigits <$> Gen.double (Range.linearFrac (-100.0) 100.0)

genJString :: Gen Value
genJString = String <$> Gen.text (Range.linear 0 20) Gen.unicode

genJArray :: Gen Value
genJArray =
  Array . V.fromList <$> Gen.list (Range.linear 0 10)
                                  (Gen.scale (`div` 4) genAnyJson)

genJObject :: Gen Value
genJObject =
  Object . HM.fromList <$> Gen.list (Range.linear 0 5) (Gen.scale (`div` 4) go)
  where
    go :: Gen (Text, Value)
    go = (,) <$> Gen.text (Range.linear 0 20) Gen.unicode <*> genAnyJson

genAnyJson :: Gen Value
genAnyJson = Gen.choice [genJBool, genJNum, genJString, genJArray, genJObject]

{-
import           Control.Exception.Safe (bracket)
import           Control.Monad (replicateM_)
import           Data.Aeson (Value (..))
import           Data.Foldable (traverse_)
import           Data.Functor.Identity (Identity)
import           Data.Int (Int64)
import           Data.Kind (Type)
import           Data.Text (take)
import           Database.Beam (Beamable, Columnar, Database, DatabaseSettings,
                                SqlInsert, Table (PrimaryKey, primaryKey),
                                TableEntity, defaultDbSettings, default_,
                                insert, insertExpressions, val_)
import           Database.Beam.MySQL (MySQL, ViaJson (ViaJson), runBeamMySQL,
                                      runInsertRowReturning)
import           Database.MySQL.Base (MySQLConn, Query (Query), close, connect,
                                      execute_)
import           Database.MySQL.Temp (toConnectInfo, withTempDB)
import           GHC.Generics (Generic)
import           Prelude hiding (take)
import           Test.Hspec (beforeAll, describe, hspec, it, shouldBe)
import           Test.QuickCheck (Gen, arbitrary, generate, liftArbitrary,
                                  oneof, scale)
import           Test.QuickCheck.Instances.Text ()
import           Test.QuickCheck.Instances.UnorderedContainers ()
import           Test.QuickCheck.Instances.Vector ()

main :: IO ()
main = withTempDB (\db -> bracket (connect . toConnectInfo $ db)
                                  close
                                  (\conn -> setUpDB conn >> spec conn))

-- Helpers

setUpDB :: MySQLConn -> IO ()
setUpDB conn = traverse_ (execute_ conn) [
  "create database test;",
  "use test",
  makeTest
  ]
  where
    makeTest :: Query
    makeTest = Query $
      "create table test_table (" <>
      "id int not null primary key auto_increment, " <>
      "from_bool varchar(255) not null, " <>
      "from_double varchar(255) not null, " <>
      "from_string varchar(255) not null, " <>
      "from_array varchar(255) not null, " <>
      "from_object varchar(255) not null" <>
      ");"

spec :: MySQLConn -> IO ()
spec conn = hspec $ do
  beforeAll go $ do
    describe "ViaJson" . replicateM_ 100 $
      it "should give back what we put in" $
        \(actual, b, d, s, arr, obj) -> do
          _testFromBool <$> actual `shouldBe` Just b
          _testFromDouble <$> actual `shouldBe` Just d
          _testFromString <$> actual `shouldBe` Just s
          _testFromArray <$> actual `shouldBe` Just arr
          _testFromObject <$> actual `shouldBe` Just obj
  where
    go :: IO (Maybe (TestT Identity),
                      ViaJson Value,
                      ViaJson Value,
                      ViaJson Value,
                      ViaJson Value,
                      ViaJson Value)
    go = do
      b <- generate (ViaJson <$> genJsonBool)
      d <- generate (ViaJson <$> genJsonNumber)
      s <- generate (ViaJson <$> genJsonString)
      arr <- generate (ViaJson <$> genJsonArray genJsonBool)
      obj <- generate (ViaJson <$> genJsonObject)
      res <- runBeamMySQL conn . runInsertRowReturning . ins b d s arr $ obj
      pure (res, b, d, s, arr, obj)
    ins ::
      ViaJson Value ->
      ViaJson Value ->
      ViaJson Value ->
      ViaJson Value ->
      ViaJson Value ->
      SqlInsert MySQL TestT
    ins b d s arr obj = insert (_testTestTable testDB) (insertExpressions [
      TestT default_ (val_ b) (val_ d) (val_ s) (val_ arr) (val_ obj)
      ])

-- Beam stuff

data TestT (f :: Type -> Type) = TestT
  { _testId         :: Columnar f Int64,
    _testFromBool   :: Columnar f (ViaJson Value),
    _testFromDouble :: Columnar f (ViaJson Value),
    _testFromString :: Columnar f (ViaJson Value),
    _testFromArray  :: Columnar f (ViaJson Value),
    _testFromObject :: Columnar f (ViaJson Value)
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

instance Table TestT where
  data PrimaryKey TestT (f :: Type -> Type) =
    TestTPK (Columnar f Int64)
      deriving stock (Generic)
      deriving anyclass (Beamable)
  primaryKey = TestTPK . _testId

deriving stock instance Eq (TestT Identity)

deriving stock instance Show (TestT Identity)

newtype TestDB (f :: Type -> Type) = TestDB
  {
    _testTestTable :: f (TableEntity TestT)
  }
  deriving stock (Generic)
  deriving anyclass (Database MySQL)

testDB :: DatabaseSettings MySQL TestDB
testDB = defaultDbSettings

-- Arbitrary stuff

genJsonBool :: Gen Value
genJsonBool = Bool <$> arbitrary

genJsonNumber :: Gen Value
genJsonNumber = Number . realToFrac <$> (arbitrary :: Gen Double)

genJsonString :: Gen Value
genJsonString = String . take 20 <$> arbitrary

genJsonArray :: Gen Value -> Gen Value
genJsonArray g = Array <$> liftArbitrary g

genJsonObject :: Gen Value
genJsonObject =
  Object <$> (scale (`div` 8) . liftArbitrary $ oneof [genJsonBool, genJsonNumber, genJsonString]) -}
