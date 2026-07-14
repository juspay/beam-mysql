{-# LANGUAGE DeriveAnyClass #-}

module DB (
  TestDB(..),
  testDB
  ) where

import qualified GHC.Records
import qualified GHC.Records.Extra
import           DB.BadSchema (BadSchemaT)
import qualified DB.BadSchema as BadSchema
import           DB.BadSchemaBig (BadSchemaBigT)
import qualified DB.BadSchemaBig as BadSchemaBig
import           DB.BadSchemaNullable (BadSchemaNullableT)
import qualified DB.BadSchemaNullable as BadSchemaNullable
import           DB.Bobby (BobbyT)
import qualified DB.Bobby as Bobby
import           DB.AltParser (AltParserT)
import qualified DB.AltParser as AltParser
import           DB.Latin1 (Latin1T)
import qualified DB.Latin1 as Latin1
import           DB.Lenient (LenientT)
import qualified DB.Lenient as Lenient
import           DB.Nullable (NullableT)
import qualified DB.Nullable as Nullable
import           DB.NullableMaybe (NullableMaybeT)
import qualified DB.NullableMaybe as NullableMaybe
import           DB.PrimaryKey.AutoInc (AutoIncT)
import qualified DB.PrimaryKey.AutoInc as AutoInc
import           DB.PrimaryKey.NoAutoInc (NoAutoIncT)
import qualified DB.PrimaryKey.NoAutoInc as NoAutoInc
import           DB.PrimaryKey.None (NoneT)
import qualified DB.PrimaryKey.None as None
import           DB.Unicode (UnicodeT)
import qualified DB.Unicode as Unicode
import           DB.ViaJSON (ViaJSONT)
import qualified DB.ViaJSON as ViaJSON
import           Data.Kind (Type)
import           Database.Beam (Database, DatabaseEntity, DatabaseModification,
                                DatabaseSettings, TableEntity, dbModification,
                                defaultDbSettings, fieldNamed,
                                modifyTableFields, setEntityName,
                                tableModification, withDbModification)
import           Database.Beam.MySQL (MySQL)
import           GHC.Generics (Generic)

data TestDB (f :: Type -> Type) = TestDB {
  nullable          :: f (TableEntity NullableT),
  nullableMaybe     :: f (TableEntity NullableMaybeT),
  viaJson           :: f (TableEntity ViaJSONT),
  bobby             :: f (TableEntity BobbyT),
  altParser         :: f (TableEntity AltParserT),
  unicode           :: f (TableEntity UnicodeT),
  pkNoAi            :: f (TableEntity NoAutoIncT),
  pkAi              :: f (TableEntity AutoIncT),
  pkAi2             :: f (TableEntity AutoIncT),
  noPk              :: f (TableEntity NoneT),
  latin1            :: f (TableEntity Latin1T),
  badSchema         :: f (TableEntity BadSchemaT),
  badSchemaBig      :: f (TableEntity BadSchemaBigT),
  badSchemaNullable :: f (TableEntity BadSchemaNullableT),
  lenient           :: f (TableEntity LenientT)
  }
  deriving stock (Generic)
  deriving anyclass (Database MySQL)

testDB :: DatabaseSettings MySQL TestDB
testDB = defaultDbSettings `withDbModification` fields `withDbModification` names
  where
    fields :: DatabaseModification (DatabaseEntity MySQL TestDB) MySQL TestDB
    fields = (dbModification @_ @MySQL) {
      nullable = modifyTableFields $
                  tableModification { Nullable.id = fieldNamed "id",
                                      Nullable.dat = fieldNamed "data" },
      nullableMaybe = modifyTableFields $
                  tableModification { NullableMaybe.id = fieldNamed "id",
                                      NullableMaybe.dat = fieldNamed "data" },
      viaJson = modifyTableFields $
                  tableModification { ViaJSON.id = fieldNamed "id",
                                      ViaJSON.fromBool = fieldNamed "from_bool",
                                      ViaJSON.fromDouble = fieldNamed "from_double",
                                      ViaJSON.fromString = fieldNamed "from_string",
                                      ViaJSON.fromArray = fieldNamed "from_array",
                                      ViaJSON.fromObject = fieldNamed "from_object" },
      bobby = modifyTableFields $
                tableModification { Bobby.id = fieldNamed "id",
                                    Bobby.badText = fieldNamed "bad_text",
                                    Bobby.badText2 = fieldNamed "bad_text2" },
      altParser = modifyTableFields $
                tableModification { AltParser.id = fieldNamed "id",
                                    AltParser.someText = fieldNamed "some_text",
                                    AltParser.someInt = fieldNamed "some_int",
                                    AltParser.someDouble = fieldNamed "some_double",
                                    AltParser.dat = fieldNamed "data" },
      unicode = modifyTableFields $
                  tableModification { Unicode.id = fieldNamed "id",
                                      Unicode.dat = fieldNamed "data" },
      pkNoAi = modifyTableFields $
                  tableModification { NoAutoInc.id = fieldNamed "id",
                                      NoAutoInc.dat = fieldNamed "data" },
      pkAi = modifyTableFields $
                  tableModification { AutoInc.id = fieldNamed "id",
                                      AutoInc.dat = fieldNamed "data" },
      pkAi2 = modifyTableFields $
                  tableModification { AutoInc.id = fieldNamed "id",
                                      AutoInc.dat = fieldNamed "data" },

      noPk = modifyTableFields $
                  tableModification { None.id = fieldNamed "id",
                                      None.dat = fieldNamed "data" },
      latin1 = modifyTableFields $
                  tableModification { Latin1.id = fieldNamed "id",
                                      Latin1.dat = fieldNamed "data" },
      badSchema = modifyTableFields $
                    tableModification { BadSchema.id = fieldNamed "id",
                                        BadSchema.dat = fieldNamed "data" },
      badSchemaBig = modifyTableFields $
                      tableModification { BadSchemaBig.id = fieldNamed "id",
                                          BadSchemaBig.field1 = fieldNamed "field_1",
                                          BadSchemaBig.field2 = fieldNamed "field_2",
                                          BadSchemaBig.field3 = fieldNamed "field_3",
                                          BadSchemaBig.field4 = fieldNamed "field_4" },
      badSchemaNullable = modifyTableFields $
                            tableModification { BadSchemaNullable.id = fieldNamed "id",
                                                BadSchemaNullable.field1 = fieldNamed "field_1",
                                                BadSchemaNullable.field2 = fieldNamed "field_2",
                                                BadSchemaNullable.field3 = fieldNamed "field_3",
                                                BadSchemaNullable.field4 = fieldNamed "field_4" },
      lenient = modifyTableFields $
                  tableModification { Lenient.id = fieldNamed "id",
                                      Lenient.int8Varchar = fieldNamed "int8_varchar",
                                      Lenient.int16Varchar = fieldNamed "int16_varchar",
                                      Lenient.int32Varchar = fieldNamed "int32_varchar",
                                      Lenient.int64Varchar = fieldNamed "int64_varchar",
                                      Lenient.floatVarchar = fieldNamed "float_varchar",
                                      Lenient.doubleVarchar = fieldNamed "double_varchar",
                                      Lenient.textTinyint = fieldNamed "text_tinyint",
                                      Lenient.textSmallint = fieldNamed "text_smallint",
                                      Lenient.textInt = fieldNamed "text_int",
                                      Lenient.textBigint = fieldNamed "text_bigint",
                                      Lenient.textFloat = fieldNamed "text_float",
                                      Lenient.int8Float = fieldNamed "int8_float",
                                      Lenient.int16Float = fieldNamed "int16_float",
                                      Lenient.int32Float = fieldNamed "int32_float",
                                      Lenient.int64Float = fieldNamed "int64_float",
                                      Lenient.textDouble = fieldNamed "text_double",
                                      Lenient.int8Double = fieldNamed "int8_double",
                                      Lenient.int16Double = fieldNamed "int16_double",
                                      Lenient.int32Double = fieldNamed "int32_double",
                                      Lenient.int64Double = fieldNamed "int64_double",
                                      Lenient.viajsonBinary = fieldNamed "viajson_binary" }
      }
    names :: DatabaseModification (DatabaseEntity MySQL TestDB) MySQL TestDB
    names = (dbModification @_ @MySQL) {
      nullable = setEntityName "nullable",
      nullableMaybe = setEntityName "nullable",
      viaJson = setEntityName "via_json",
      bobby = setEntityName "bobby",
      altParser = setEntityName "alt_parser",
      unicode = setEntityName "unicode",
      pkNoAi = setEntityName "pk_no_ai",
      pkAi = setEntityName "pk_ai",
      pkAi2 = setEntityName "pk_ai2",
      noPk = setEntityName "no_pk",
      latin1 = setEntityName "latin1",
      badSchema = setEntityName "bad_schema",
      badSchemaBig = setEntityName "bad_schema_big",
      badSchemaNullable = setEntityName "bad_schema_nullable",
      lenient = setEntityName "lenient"
      }
