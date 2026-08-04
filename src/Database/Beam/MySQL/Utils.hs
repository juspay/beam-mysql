{-# LANGUAGE MultiWayIf #-}

module Database.Beam.MySQL.Utils where

import qualified GHC.Records
import qualified GHC.Records.Extra
import           Data.Text (Text)
import           Database.MySQL.Base (FieldType, mySQLTypeBit, mySQLTypeBlob,
                                      mySQLTypeDate, mySQLTypeDateTime,
                                      mySQLTypeDateTime2, mySQLTypeDecimal,
                                      mySQLTypeDouble, mySQLTypeEnum,
                                      mySQLTypeFloat, mySQLTypeInt24,
                                      mySQLTypeLong, mySQLTypeLongBlob,
                                      mySQLTypeLongLong, mySQLTypeMediumBlob,
                                      mySQLTypeNewDate, mySQLTypeNewDecimal,
                                      mySQLTypeNull, mySQLTypeSet,
                                      mySQLTypeShort, mySQLTypeString,
                                      mySQLTypeTime, mySQLTypeTime2,
                                      mySQLTypeTimestamp, mySQLTypeTimestamp2,
                                      mySQLTypeTiny, mySQLTypeTinyBlob,
                                      mySQLTypeVarChar, mySQLTypeVarString,
                                      mySQLTypeYear)

toSQLTypeName :: FieldType -> Text
toSQLTypeName ft = "MySQL " <>
  if | ft == mySQLTypeDecimal -> "Decimal"
     | ft == mySQLTypeTiny -> "Tiny"
     | ft == mySQLTypeShort -> "Short"
     | ft == mySQLTypeLong -> "Long"
     | ft == mySQLTypeFloat -> "Float"
     | ft == mySQLTypeDouble -> "Double"
     | ft == mySQLTypeNull -> "Null"
     | ft == mySQLTypeTimestamp -> "Timestamp"
     | ft == mySQLTypeLongLong -> "LongLong"
     | ft == mySQLTypeInt24 -> "Int24"
     | ft == mySQLTypeDate -> "Date"
     | ft == mySQLTypeTime -> "Time"
     | ft == mySQLTypeDateTime -> "DateTime"
     | ft == mySQLTypeYear -> "Year"
     | ft == mySQLTypeNewDate -> "NewDate"
     | ft == mySQLTypeVarChar -> "VarChar"
     | ft == mySQLTypeBit -> "Bit"
     | ft == mySQLTypeTimestamp2 -> "Timestamp2"
     | ft == mySQLTypeDateTime2 -> "DateTime2"
     | ft == mySQLTypeTime2 -> "Time2"
     | ft == mySQLTypeNewDecimal -> "NewDecimal"
     | ft == mySQLTypeEnum -> "Enum"
     | ft == mySQLTypeSet -> "Set"
     | ft == mySQLTypeTinyBlob -> "TinyBlob"
     | ft == mySQLTypeMediumBlob -> "MediumBlob"
     | ft == mySQLTypeLongBlob -> "LongBlob"
     | ft == mySQLTypeBlob -> "Blob"
     | ft == mySQLTypeVarString -> "VarString"
     | ft == mySQLTypeString -> "String"
     | otherwise -> "Geometry"
