{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeFamilies   #-}

module Main (main) where

import qualified GHC.Records
import qualified GHC.Records.Extra
import           Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), withText)
import           Data.Aeson.Types (Parser)
import           Data.Functor.Identity (Identity)
import           Data.Int (Int64)
import           Data.Kind (Type)
import           Data.Text (Text)
import           Database.Beam (Beamable, Columnar, Database, DatabaseSettings,
                                Table (PrimaryKey, primaryKey), TableEntity,
                                defaultDbSettings)
import           Database.Beam.MySQL (MySQL, ViaJson (ViaJson), dumpDeleteSQL,
                                      dumpInsertSQL, dumpSelectSQL,
                                      dumpUpdateSQL)
import           Database.Beam.Query (QBaseScope, QExpr, QFieldAssignment,
                                      SqlDelete, SqlInsert, SqlSelect,
                                      SqlUpdate, all_, default_, delete,
                                      filter_, guard_, insert,
                                      insertExpressions, insertValues,
                                      isNothing_, select, toNewValue,
                                      toOldValue, updateTable, val_, (==.))
import           GHC.Generics (Generic)
import           Test.Tasty (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit (Assertion, assertEqual, testCase)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "SQL generation" [
  simpleSelectTests,
  operatorSelectTests,
  insertTests,
  updateTests,
  deleteTests
  ]

simpleSelectTests :: TestTree
simpleSelectTests = testGroup "simple SELECTs" [
  testCase "no extras" noExtras,
  testCase "with WHERE" withWhere,
  testCase "escaping WHERE literals" selectEscaping,
  testCase "escaping WHERE literals via JSON" selectEscapingJson
  ]
  where
    noExtras :: Assertion
    noExtras = assertEqual "" simpleSelectSQL . dumpSelectSQL $ simpleSelect
    withWhere :: Assertion
    withWhere = assertEqual "" whereSelectSQL . dumpSelectSQL $ whereSelect
    selectEscaping :: Assertion
    selectEscaping = assertEqual "" escapeSelectSQL . dumpSelectSQL $ escapeSelect
    selectEscapingJson :: Assertion
    selectEscapingJson = assertEqual "" escapeSelectJsonSQL . dumpSelectSQL $ escapeSelectJson

operatorSelectTests :: TestTree
operatorSelectTests = testGroup "SELECTs with operators" [
  testCase "with IS NULL" withIsNull
  ]
  where
    withIsNull :: Assertion
    withIsNull = assertEqual "" isNullSelectSQL . dumpSelectSQL $ isNullSelect

insertTests :: TestTree
insertTests = testGroup "INSERTs" [
  testCase "VALUES" values,
  testCase "multi-value" multiValue,
  testCase "escaping WHERE literals" insertEscaping,
  testCase "escaping WHERE literals via JSON" insertEscapingJson
  ]
  where
    values :: Assertion
    values = assertEqual "" simpleInsertSQL . dumpInsertSQL $ simpleInsert
    multiValue :: Assertion
    multiValue = assertEqual "" multiInsertSQL . dumpInsertSQL $ multiInsert
    insertEscaping :: Assertion
    insertEscaping = assertEqual "" insertEscapingSQL . dumpInsertSQL $ insertEscaping'
    insertEscapingJson :: Assertion
    insertEscapingJson =
      assertEqual "" escapingInsertJsonSQL . dumpInsertSQL $ escapingInsertJson

updateTests :: TestTree
updateTests = testGroup "UPDATEs" [
  testCase "escaping WHERE literals" updateEscaping,
  testCase "escaping WHERE literals via JSON" updateEscapingJson
  ]
  where
    updateEscaping :: Assertion
    updateEscaping = assertEqual "" escapingUpdateSQL . dumpUpdateSQL $ escapingUpdate
    updateEscapingJson :: Assertion
    updateEscapingJson =
      assertEqual "" escapingUpdateJsonSQL . dumpUpdateSQL $ escapingUpdateJson

deleteTests :: TestTree
deleteTests = testGroup "DELETEs" [
  testCase "escaping WHERE literals" deleteEscaping,
  testCase "escaping WHERE literals via JSON" deleteEscapingJson
  ]
  where
    deleteEscaping :: Assertion
    deleteEscaping = assertEqual "" escapingDeleteSQL . dumpDeleteSQL $ escapingDelete
    deleteEscapingJson :: Assertion
    deleteEscapingJson =
      assertEqual "" escapingDeleteJsonSQL . dumpDeleteSQL $ escapingDeleteJson

-- Helpers

-- Beam boilerplate

bobbyTables :: Text
bobbyTables = "\'; DROP TABLE students; --"

data Evil = Evil
  deriving stock (Eq, Show)

instance ToJSON Evil where
  toJSON _ = toJSON bobbyTables

instance FromJSON Evil where
  parseJSON = withText "Evil" go
    where
      go :: Text -> Parser Evil
      go t =
        if t == bobbyTables
        then pure Evil
        else fail "Not possible"

data TestT (f :: Type -> Type) = TestT
  { _testId   :: Columnar f Int64,
    _testData :: Columnar f (Maybe Text)
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

instance Table TestT where
  data PrimaryKey TestT (f :: Type -> Type) =
    TestTPK (Columnar f Int64)
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = TestTPK . _testId

newtype JsonT (f :: Type -> Type) = JsonT
  {
    _testJson :: Columnar f (ViaJson Evil)
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

instance Table JsonT where
  data PrimaryKey JsonT (f :: Type -> Type) =
    JsonTPK (Columnar f (ViaJson Evil))
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = JsonTPK . _testJson

data TestDB (f :: Type -> Type) = TestDB
  {
    _testTestTable :: f (TableEntity TestT),
    _testJsonTable :: f (TableEntity JsonT)
  }
  deriving stock (Generic)
  deriving anyclass (Database MySQL)

testDB :: DatabaseSettings MySQL TestDB
testDB = defaultDbSettings

-- Selects

simpleSelect :: SqlSelect MySQL (TestT Identity)
simpleSelect = select . all_ . _testTestTable $ testDB

simpleSelectSQL :: Maybe Text
simpleSelectSQL = Just $
  "SELECT `t0`.`id` AS `res0`, `t0`.`data` AS `res1` " <>
  "FROM `test_table` AS `t0`;"

whereSelect :: SqlSelect MySQL (TestT Identity)
whereSelect = select $ do
  res <- all_ . _testTestTable $ testDB
  guard_ (_testData res ==. val_ (Just "foo"))
  pure res

whereSelectSQL :: Maybe Text
whereSelectSQL = Just $
  "SELECT `t0`.`id` AS `res0`, `t0`.`data` AS `res1` " <>
  "FROM `test_table` AS `t0` " <>
  "WHERE CASE WHEN ((`t0`.`data`) IS NULL) AND (('foo') IS NULL) " <>
  "THEN TRUE " <>
  "WHEN ((`t0`.`data`) IS NULL) OR (('foo') IS NULL) " <>
  "THEN FALSE ELSE (`t0`.`data`) = ('foo') END;"

escapeSelect :: SqlSelect MySQL (TestT Identity)
escapeSelect = select . filter_ go . all_ . _testTestTable $ testDB
  where
    go :: TestT (QExpr MySQL QBaseScope) -> QExpr MySQL QBaseScope Bool
    go row = _testData row ==. val_ (Just "foo\"")

escapeSelectSQL :: Maybe Text
escapeSelectSQL = Just $
  "SELECT `t0`.`id` AS `res0`, `t0`.`data` AS `res1` " <>
  "FROM `test_table` AS `t0` " <>
  "WHERE CASE WHEN ((`t0`.`data`) IS NULL) AND (('foo\\\"') IS NULL) " <>
  "THEN TRUE " <>
  "WHEN ((`t0`.`data`) IS NULL) OR (('foo\\\"') IS NULL) " <>
  "THEN FALSE ELSE (`t0`.`data`) = ('foo\\\"') END;"

isNullSelect :: SqlSelect MySQL (TestT Identity)
isNullSelect = select $ do
  res <- all_ . _testTestTable $ testDB
  guard_ (isNothing_ . _testData $ res)
  pure res

isNullSelectSQL :: Maybe Text
isNullSelectSQL = Just $
  "SELECT `t0`.`id` AS `res0`, `t0`.`data` AS `res1` " <>
  "FROM `test_table` AS `t0` " <>
  "WHERE (`t0`.`data`) IS NULL;"

escapeSelectJsonSQL :: Maybe Text
escapeSelectJsonSQL = Just $
  "SELECT `t0`.`json` AS `res0` " <>
  "FROM `json_table` AS `t0` " <>
  "WHERE (`t0`.`json`) = ('\\\"\\'; DROP TABLE students; --\\\"');"

escapeSelectJson :: SqlSelect MySQL (JsonT Identity)
escapeSelectJson = select . filter_ go . all_ . _testJsonTable $ testDB
  where
    go :: JsonT (QExpr MySQL QBaseScope) -> QExpr MySQL QBaseScope Bool
    go row = _testJson row ==. val_ (ViaJson Evil)

-- Inserts

simpleInsert :: SqlInsert MySQL TestT
simpleInsert =
  insert (_testTestTable testDB) (insertExpressions [go])
  where
    go :: forall s . TestT (QExpr MySQL s)
    go = TestT default_ (val_ . Just $ "foo")

simpleInsertSQL :: Maybe Text
simpleInsertSQL = Just "INSERT INTO `test_table` (`id`, `data`) VALUES (DEFAULT, 'foo');"

multiInsert :: SqlInsert MySQL TestT
multiInsert =
  insert (_testTestTable testDB) (insertExpressions go)
  where
    go :: forall s . [TestT (QExpr MySQL s)]
    go = [
      TestT default_ (val_ . Just $ "foo"),
      TestT default_ (val_ . Just $ "bar")
      ]

multiInsertSQL :: Maybe Text
multiInsertSQL = Just "INSERT INTO `test_table` (`id`, `data`) VALUES (DEFAULT, 'foo'), (DEFAULT, 'bar');"

insertEscaping' :: SqlInsert MySQL TestT
insertEscaping' = insert (_testTestTable testDB) (insertExpressions go)
  where
    go :: forall s . [TestT (QExpr MySQL s)]
    go = [
      TestT default_ (val_ . Just $ "foo\"")
      ]

insertEscapingSQL :: Maybe Text
insertEscapingSQL = Just "INSERT INTO `test_table` (`id`, `data`) VALUES (DEFAULT, 'foo\\\"');"

escapingInsertJsonSQL :: Maybe Text
escapingInsertJsonSQL = Just $
  "INSERT INTO `json_table` (`json`) " <>
  "VALUES ('\\\"\\'; DROP TABLE students; --\\\"');"

escapingInsertJson :: SqlInsert MySQL JsonT
escapingInsertJson =
  insert (_testJsonTable testDB) . insertValues $ [
    JsonT (ViaJson Evil)
    ]

-- Updates

escapingUpdateSQL :: Maybe Text
escapingUpdateSQL = Just $
  "UPDATE `test_table` SET `data`='bar\\\"' " <>
  "WHERE CASE WHEN ((`data`) IS NULL) AND (('foo\\\"') IS NULL) " <>
  "THEN TRUE " <>
  "WHEN ((`data`) IS NULL) OR (('foo\\\"') IS NULL) " <>
  "THEN FALSE ELSE (`data`) = ('foo\\\"') END;"

escapingUpdate :: SqlUpdate MySQL TestT
escapingUpdate = updateTable (_testTestTable testDB) upd wher
  where
    upd :: TestT (QFieldAssignment MySQL TestT)
    upd = TestT toOldValue (toNewValue (val_ (Just "bar\"")))
    wher :: forall s . TestT (QExpr MySQL s) -> QExpr MySQL s Bool
    wher row = _testData row ==. val_ (Just "foo\"")

escapingUpdateJsonSQL :: Maybe Text
escapingUpdateJsonSQL = Just $
  "UPDATE `json_table` " <>
  "SET `json`='\\\"\\'; DROP TABLE students; --\\\"' " <>
  "WHERE (`json`) = ('\\\"\\'; DROP TABLE students; --\\\"');"

escapingUpdateJson :: SqlUpdate MySQL JsonT
escapingUpdateJson = updateTable (_testJsonTable testDB) upd wher
  where
    upd :: JsonT (QFieldAssignment MySQL JsonT)
    upd = JsonT (toNewValue (val_ (ViaJson Evil)))
    wher :: forall s . JsonT (QExpr MySQL s) -> QExpr MySQL s Bool
    wher row = _testJson row ==. val_ (ViaJson Evil)

-- Deletes

escapingDeleteSQL :: Maybe Text
escapingDeleteSQL = Just $
  "DELETE FROM `test_table` " <>
  "WHERE CASE WHEN ((`data`) IS NULL) AND (('foo\\\"') IS NULL) " <>
  "THEN TRUE " <>
  "WHEN ((`data`) IS NULL) OR (('foo\\\"') IS NULL) " <>
  "THEN FALSE ELSE (`data`) = ('foo\\\"') END;"

escapingDelete :: SqlDelete MySQL TestT
escapingDelete = delete (_testTestTable testDB) go
  where
    go :: (forall s' . TestT (QExpr MySQL s')) -> QExpr MySQL s Bool
    go row = _testData row ==. val_ (Just "foo\"")

escapingDeleteJsonSQL :: Maybe Text
escapingDeleteJsonSQL = Just $
  "DELETE FROM `json_table` " <>
  "WHERE (`json`) = ('\\\"\\'; DROP TABLE students; --\\\"');"

escapingDeleteJson :: SqlDelete MySQL JsonT
escapingDeleteJson = delete (_testJsonTable testDB) go
  where
    go :: (forall s' . JsonT (QExpr MySQL s')) -> QExpr MySQL s Bool
    go row = _testJson row ==. val_ (ViaJson Evil)

