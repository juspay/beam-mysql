{-# LANGUAGE TypeOperators #-}
-- Due to RDP plugin
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Beam.MySQL.Syntax.Misc where

import           Data.Text (Text)
import           Database.Beam.Backend.SQL (IsSql92AggregationSetQuantifierSyntax (..),
                                            IsSql92ExtractFieldSyntax (..),
                                            IsSql92FieldNameSyntax (..),
                                            IsSql92QuantifierSyntax (..))

-- | Representation of a qualified or unqualified field name.
--
-- @since 1.2.3.1
data MySQLFieldNameSyntax =
  -- | A field qualified with the name of its table.
  --
  -- @since 1.2.3.1
  QualifiedField {
    table :: {-# UNPACK #-} !Text,
    field :: {-# UNPACK #-} !Text
    } |
  -- | A field without any table associated with it.
  --
  -- @since 1.2.3.1
  UnqualifiedField {
    field :: {-# UNPACK #-} !Text
    }
  deriving stock (
    -- | @since 1.2.3.1
    Eq
    ,
    -- | @since 1.2.3.1
    Show)

-- | @since 1.2.3.1
instance IsSql92FieldNameSyntax MySQLFieldNameSyntax where
  {-# INLINABLE qualifiedField #-}
  qualifiedField :: Text -> Text -> MySQLFieldNameSyntax
  qualifiedField = QualifiedField
  {-# INLINABLE unqualifiedField #-}
  unqualifiedField :: Text -> MySQLFieldNameSyntax
  unqualifiedField = UnqualifiedField

data MySQLQuantifierSyntax = All | Any
 deriving stock (Eq, Show)

instance IsSql92QuantifierSyntax MySQLQuantifierSyntax where
  {-# INLINABLE quantifyOverAll #-}
  quantifyOverAll :: MySQLQuantifierSyntax
  quantifyOverAll = All
  {-# INLINABLE quantifyOverAny #-}
  quantifyOverAny :: MySQLQuantifierSyntax
  quantifyOverAny = Any

data MySQLExtractFieldSyntax =
  SecondsField |
  MinutesField |
  HourField |
  DayField |
  MonthField |
  YearField
  deriving stock (Eq, Show)

instance IsSql92ExtractFieldSyntax MySQLExtractFieldSyntax where
  {-# INLINABLE secondsField #-}
  secondsField :: MySQLExtractFieldSyntax
  secondsField = SecondsField
  {-# INLINABLE minutesField #-}
  minutesField :: MySQLExtractFieldSyntax
  minutesField = MinutesField
  {-# INLINABLE hourField #-}
  hourField :: MySQLExtractFieldSyntax
  hourField = HourField
  {-# INLINABLE dayField #-}
  dayField :: MySQLExtractFieldSyntax
  dayField = DayField
  {-# INLINABLE monthField #-}
  monthField :: MySQLExtractFieldSyntax
  monthField = MonthField
  {-# INLINABLE yearField #-}
  yearField :: MySQLExtractFieldSyntax
  yearField = YearField

data MySQLAggregationSetQuantifierSyntax =
  SetDistinct |
  SetAll
  deriving stock (Eq, Show)

instance IsSql92AggregationSetQuantifierSyntax MySQLAggregationSetQuantifierSyntax where
  {-# INLINABLE setQuantifierDistinct #-}
  setQuantifierDistinct :: MySQLAggregationSetQuantifierSyntax
  setQuantifierDistinct = SetDistinct
  {-# INLINABLE setQuantifierAll #-}
  setQuantifierAll :: MySQLAggregationSetQuantifierSyntax
  setQuantifierAll = SetAll
