-- Due to RDP plugin
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Database.Beam.MySQL.Syntax.DataType where

import           Data.Text (Text)
import           Database.Beam.Backend.SQL (IsSql92DataTypeSyntax (..))
import           Prelude hiding (length)

data MySQLPrecision =
  None |
  DigitsOnly {-# UNPACK #-} !Word |
  DigitsScale {-# UNPACK #-} !Word {-# UNPACK #-} !Word
  deriving stock (Eq, Show)

intoMySQLPrecision :: Maybe (Word, Maybe Word) -> MySQLPrecision
intoMySQLPrecision = \case
  Nothing -> None
  Just (d, mn) -> case mn of
    Nothing -> DigitsOnly d
    Just n  -> DigitsScale d n

data MySQLDataTypeSyntax =
  DomainType {-# UNPACK #-} !Text |
  CharType {
    length       :: !(Maybe Word),
    characterSet :: !(Maybe Text)
    } |
  VarCharType {
    length       :: !(Maybe Word),
    characterSet :: !(Maybe Text)
    } |
  NationalCharType {
    length :: !(Maybe Word)
    } |
  NationalVarCharType {
    length :: !(Maybe Word)
    } |
  BitType {
    length :: !(Maybe Word)
    } |
  VarBitType {
    length :: !(Maybe Word)
    } |
  NumericType !MySQLPrecision |
  DecimalType !MySQLPrecision |
  IntType |
  SmallIntType |
  FloatType {
    precision :: !(Maybe Word)
    } |
  DoubleType |
  RealType |
  DateType |
  TimeType |
  TimestampType
  deriving stock (Eq, Show)

instance IsSql92DataTypeSyntax MySQLDataTypeSyntax where
  {-# INLINABLE domainType #-}
  domainType :: Text -> MySQLDataTypeSyntax
  domainType = DomainType
  {-# INLINABLE charType #-}
  charType :: Maybe Word -> Maybe Text -> MySQLDataTypeSyntax
  charType = CharType
  {-# INLINABLE varCharType #-}
  varCharType :: Maybe Word -> Maybe Text -> MySQLDataTypeSyntax
  varCharType = VarCharType
  {-# INLINABLE nationalCharType #-}
  nationalCharType :: Maybe Word -> MySQLDataTypeSyntax
  nationalCharType = NationalCharType
  {-# INLINABLE nationalVarCharType #-}
  nationalVarCharType :: Maybe Word -> MySQLDataTypeSyntax
  nationalVarCharType = NationalVarCharType
  {-# INLINABLE bitType #-}
  bitType :: Maybe Word -> MySQLDataTypeSyntax
  bitType = BitType
  {-# INLINABLE varBitType #-}
  varBitType :: Maybe Word -> MySQLDataTypeSyntax
  varBitType = VarBitType
  {-# INLINABLE numericType #-}
  numericType :: Maybe (Word, Maybe Word) -> MySQLDataTypeSyntax
  numericType = NumericType . intoMySQLPrecision
  {-# INLINABLE decimalType #-}
  decimalType :: Maybe (Word, Maybe Word) -> MySQLDataTypeSyntax
  decimalType = DecimalType . intoMySQLPrecision
  {-# INLINABLE intType #-}
  intType :: MySQLDataTypeSyntax
  intType = IntType
  {-# INLINABLE smallIntType #-}
  smallIntType :: MySQLDataTypeSyntax
  smallIntType = SmallIntType
  {-# INLINABLE floatType #-}
  floatType :: Maybe Word -> MySQLDataTypeSyntax
  floatType = FloatType
  {-# INLINABLE doubleType #-}
  doubleType :: MySQLDataTypeSyntax
  doubleType = DoubleType
  {-# INLINABLE realType #-}
  realType :: MySQLDataTypeSyntax
  realType = RealType
  {-# INLINABLE dateType #-}
  dateType :: MySQLDataTypeSyntax
  dateType = DateType
  {-# INLINABLE timeType #-}
  timeType :: Maybe Word -> Bool -> MySQLDataTypeSyntax
  timeType _ _ = TimeType
  {-# INLINABLE timestampType #-}
  timestampType :: Maybe Word -> Bool -> MySQLDataTypeSyntax
  timestampType _ _ = TimestampType
