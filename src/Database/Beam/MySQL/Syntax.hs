{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Database.Beam.MySQL.Syntax
(
  MysqlSyntax (..),
  MysqlTableNameSyntax (..),
  MysqlInsertValuesSyntax (..),
  MysqlInsertSyntax (..),
  intoQuery, intoDebugText, intoTableName, bracketWrap,
  defaultE, backtickWrap, intoLazyText, textSyntax
) where

import           Data.Aeson (Value)
import           Data.Aeson.Text (encodeToTextBuilder)
import           Data.ByteString (ByteString)
import           Data.Fixed (E12, Fixed, showFixed)
import           Data.Foldable (fold)
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import           Data.Int (Int16, Int32, Int64, Int8)
import           Data.List (intersperse)
import           Data.Scientific (Scientific)
import           Data.String (IsString, fromString)
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Builder (Builder, fromLazyText, fromText,
                                         toLazyText)
import           Data.Text.Lazy.Builder.Scientific (scientificBuilder)
import qualified Data.Text.Lazy.Encoding as TLE
import           Data.Time.Calendar (Day)
import           Data.Time.Clock (NominalDiffTime)
import           Data.Time.Format.ISO8601 (formatShow, iso8601Format)
import           Data.Time.LocalTime (LocalTime, TimeOfDay, localDay,
                                      localTimeOfDay)
import           Data.Word (Word16, Word32, Word64, Word8)
import           Database.Beam.Backend.SQL (HasSqlValueSyntax (..), IsSql92AggregationExpressionSyntax (..),
                                            IsSql92AggregationSetQuantifierSyntax (..),
                                            IsSql92DataTypeSyntax (..),
                                            IsSql92DeleteSyntax (..),
                                            IsSql92ExpressionSyntax (..),
                                            IsSql92ExtractFieldSyntax (..),
                                            IsSql92FieldNameSyntax (..),
                                            IsSql92FromSyntax (..),
                                            IsSql92GroupingSyntax (..),
                                            IsSql92InsertSyntax (..),
                                            IsSql92InsertValuesSyntax (..),
                                            IsSql92OrderingSyntax (..),
                                            IsSql92ProjectionSyntax (..),
                                            IsSql92QuantifierSyntax (..),
                                            IsSql92SelectSyntax (..),
                                            IsSql92SelectTableSyntax (..),
                                            IsSql92Syntax (..),
                                            IsSql92TableNameSyntax (..),
                                            IsSql92TableSourceSyntax (..),
                                            IsSql92UpdateSyntax (..),
                                            IsSql99ConcatExpressionSyntax (..),
                                            SqlNull (..))
import           Database.MySQL.Base (Query (..), fromQuery)
import           Database.MySQL.Protocol.Escape (escapeText)
import           Fmt (build, unlinesF, whenF, (+|), (|+))

-- General syntax type
newtype MysqlSyntax = MysqlSyntax (HashSet Text, Builder)
  deriving newtype (Semigroup, Monoid, Eq)

instance IsString MysqlSyntax where
 fromString = MysqlSyntax . (HS.empty,) . fromString

intoQuery :: MysqlSyntax -> Query
intoQuery (MysqlSyntax (_, b)) = Query . TLE.encodeUtf8 . toLazyText $ b

intoDebugText :: MysqlSyntax -> Text
intoDebugText (MysqlSyntax (tables, b)) =
  "Query: " +|
  toLazyText b |+
  ", relevant table(s):" +|
  unlinesF tables |+
  ""

intoLazyText :: MysqlSyntax -> TL.Text
intoLazyText (MysqlSyntax (_, b)) = toLazyText b

charLen :: Maybe Word -> MysqlSyntax
charLen =
  MysqlSyntax . (HS.empty,) . maybe "MAX" (fromString . show)

charSet :: Maybe Text -> MysqlSyntax
charSet = MysqlSyntax . (HS.empty,) . foldMap go
  where
    go :: Text -> Builder
    go = (" CHARACTER SET " <>) . backtickWrap . fromText

numPrec :: Maybe (Word, Maybe Word) -> MysqlSyntax
numPrec = \case
  Nothing -> mempty
  Just (d, mn) -> MysqlSyntax . (HS.empty,) . bracketWrap $ case mn of
    Nothing -> fromString . show $ d
    Just n  -> (fromString . show $ d) <> ", " <> (fromString . show $ n)

textSyntax :: Text -> MysqlSyntax
textSyntax = MysqlSyntax . (HS.empty,) . fromText

-- Combination of data type name and its casting
data MysqlDataTypeSyntax = MysqlDataTypeSyntax {
  _mysqlDataType :: !MysqlSyntax,
  mysqlTypeCast  :: !MysqlSyntax
  }
  deriving stock (Eq)

-- Table name with optional database name
data MysqlTableNameSyntax = MysqlTableNameSyntax
  !(Maybe Text)
  {-# UNPACK #-} !Text
  deriving stock (Eq)

intoTableName :: MysqlTableNameSyntax -> MysqlSyntax
intoTableName (MysqlTableNameSyntax db name) =
  MysqlSyntax . (HS.singleton name,) $
    foldMap go db <> (backtickWrap . fromText $ name)
  where
    go :: Text -> Builder
    go t = (backtickWrap . fromText $ t) <> "."

-- Insert values syntax to support runInsertRowReturning
data MysqlInsertValuesSyntax =
  FromExprs [[MysqlSyntax]] |
  FromSQL MysqlSyntax
  deriving stock (Eq)

-- Insert syntax to support runInsertRowReturning
data MysqlInsertSyntax =
  Insert MysqlTableNameSyntax [Text] MysqlInsertValuesSyntax
  deriving stock (Eq)

-- How we convert everything types defined in FromField to MySQL syntax

instance HasSqlValueSyntax MysqlSyntax Bool where
  sqlValueSyntax = \case
    True -> "TRUE"
    False -> "FALSE"

instance HasSqlValueSyntax MysqlSyntax Int8 where
  sqlValueSyntax = fromString . show

instance HasSqlValueSyntax MysqlSyntax Int16 where
  sqlValueSyntax = fromString . show

instance HasSqlValueSyntax MysqlSyntax Int32 where
  sqlValueSyntax = fromString . show

instance HasSqlValueSyntax MysqlSyntax Int64 where
  sqlValueSyntax = fromString . show

instance HasSqlValueSyntax MysqlSyntax Int where
  sqlValueSyntax = fromString . show

instance HasSqlValueSyntax MysqlSyntax Word8 where
  sqlValueSyntax = fromString . show

instance HasSqlValueSyntax MysqlSyntax Word16 where
  sqlValueSyntax = fromString . show

instance HasSqlValueSyntax MysqlSyntax Word32 where
  sqlValueSyntax = fromString . show

instance HasSqlValueSyntax MysqlSyntax Word64 where
  sqlValueSyntax = fromString . show

instance HasSqlValueSyntax MysqlSyntax Word where
  sqlValueSyntax = fromString . show

instance HasSqlValueSyntax MysqlSyntax Float where
  sqlValueSyntax = fromString . show

instance HasSqlValueSyntax MysqlSyntax Double where
  sqlValueSyntax = fromString . show

instance HasSqlValueSyntax MysqlSyntax Scientific where
  sqlValueSyntax = MysqlSyntax . (HS.empty,) . scientificBuilder

-- Rational is exempted, because there's no good way to do this really

instance (HasSqlValueSyntax MysqlSyntax a) =>
  HasSqlValueSyntax MysqlSyntax (Maybe a) where
  sqlValueSyntax = maybe (sqlValueSyntax SqlNull) sqlValueSyntax

instance HasSqlValueSyntax MysqlSyntax SqlNull where
  sqlValueSyntax = const "NULL"

instance HasSqlValueSyntax MysqlSyntax ByteString where
  sqlValueSyntax = quoteWrap . textSyntax . decodeUtf8

instance HasSqlValueSyntax MysqlSyntax Text where
  sqlValueSyntax = quoteWrap . textSyntax . escapeText

instance HasSqlValueSyntax MysqlSyntax Day where
  sqlValueSyntax =
    MysqlSyntax . (HS.empty,) . quoteWrap . fromString . formatShow iso8601Format

instance HasSqlValueSyntax MysqlSyntax TimeOfDay where
  sqlValueSyntax =
    MysqlSyntax . (HS.empty,) . quoteWrap . fromString . formatShow iso8601Format

instance HasSqlValueSyntax MysqlSyntax LocalTime where
  sqlValueSyntax lt =
    MysqlSyntax . (HS.empty,) . quoteWrap $ (day <> " " <> tod)
    where
      day = fromString . formatShow iso8601Format . localDay $ lt
      tod = fromString . formatShow iso8601Format . localTimeOfDay $ lt

instance HasSqlValueSyntax MysqlSyntax NominalDiffTime where
  sqlValueSyntax d = textSyntax time
    where
      dWhole :: Int
      dWhole = abs . floor $ d
      hours :: Int
      hours = dWhole `div` 3600
      d' :: Int
      d' = dWhole - (hours * 3600)
      minutes :: Int
      minutes = d' `div` 60
      seconds :: NominalDiffTime
      seconds = abs d - fromIntegral ((hours * 3600) + (minutes * 60))
      secondsFixed :: Fixed E12
      secondsFixed = fromRational . toRational $ seconds
      time :: Text
      time = whenF (d < 0) (build @Text "-") +|
              whenF (hours < 10) (build @Text "0") +|
              hours |+
              ":" +|
              whenF (minutes < 10) (build @Text "0") +|
              minutes |+
              ":" +|
              whenF (secondsFixed < 10) (build @Text "0") +|
              showFixed False secondsFixed |+
              ""

instance HasSqlValueSyntax MysqlSyntax Value where
  sqlValueSyntax =
    MysqlSyntax . (HS.empty,) . quoteWrap . encodeToTextBuilder

-- Extra convenience instances

instance HasSqlValueSyntax MysqlSyntax Integer where
  sqlValueSyntax = fromString . show

instance HasSqlValueSyntax MysqlSyntax String where
  sqlValueSyntax =
    MysqlSyntax . (HS.empty,) . quoteWrap . fromString

instance HasSqlValueSyntax MysqlSyntax TL.Text where
  sqlValueSyntax =
    MysqlSyntax . (HS.empty,) . quoteWrap . fromLazyText

-- Syntax defs

instance IsSql92FieldNameSyntax MysqlSyntax where
  unqualifiedField = backtickWrap . textSyntax
  qualifiedField qual f =
    (backtickWrap . textSyntax $ qual) <>
    "." <>
    unqualifiedField f

instance IsSql92QuantifierSyntax MysqlSyntax where
  quantifyOverAll = "ALL"
  quantifyOverAny = "ANY"

instance IsSql92DataTypeSyntax MysqlDataTypeSyntax where
  domainType t = MysqlDataTypeSyntax go go
    where
      go = backtickWrap . textSyntax $ t
  charType mlen mcs = MysqlDataTypeSyntax def "CHAR"
    where
      def = "CHAR" <> charLen mlen <> charSet mcs
  varCharType mlen mcs = MysqlDataTypeSyntax def "CHAR"
    where
      def = "VARCHAR" <> charLen mlen <> charSet mcs
  nationalCharType mlen = MysqlDataTypeSyntax def "CHAR"
    where
      def = "NATIONAL CHAR" <> charLen mlen
  nationalVarCharType mlen = MysqlDataTypeSyntax def "CHAR"
    where
      def = "NATIONAL CHAR VARYING" <> charLen mlen
  bitType mlen = MysqlDataTypeSyntax def "BINARY"
    where
      def = "BIT" <> charLen mlen
  varBitType mlen = MysqlDataTypeSyntax def "BINARY"
    where
      def = "VARBINARY" <> charLen mlen
  numericType mprec =
    MysqlDataTypeSyntax ("NUMERIC" <> prec) ("DECIMAL" <> prec)
    where
      prec = numPrec mprec
  decimalType mprec = MysqlDataTypeSyntax go go
    where
      go = "DECIMAL" <> numPrec mprec
  intType = MysqlDataTypeSyntax "INT" "INTEGER"
  smallIntType = MysqlDataTypeSyntax "SMALL INT" "INTEGER"
  floatType mprec = MysqlDataTypeSyntax def "DECIMAL"
    where
      def = MysqlSyntax . (HS.empty,) $
        "FLOAT" <> foldMap (bracketWrap . fromString . show) mprec
  doubleType = MysqlDataTypeSyntax "DOUBLE" "DECIMAL"
  realType = MysqlDataTypeSyntax "REAL" "DECIMAL"
  dateType = MysqlDataTypeSyntax "DATE" "DATE"
  timeType _ _ = MysqlDataTypeSyntax "TIME" "TIME"
  timestampType _ _ = MysqlDataTypeSyntax "TIMESTAMP" "DATETIME"

instance IsSql92ExtractFieldSyntax MysqlSyntax where
  secondsField = "SECOND"
  minutesField = "MINUTE"
  hourField = "HOUR"
  dayField = "DAY"
  monthField = "MONTH"
  yearField = "YEAR"

instance IsSql92ExpressionSyntax MysqlSyntax where
  type Sql92ExpressionValueSyntax MysqlSyntax = MysqlSyntax
  type Sql92ExpressionFieldNameSyntax MysqlSyntax = MysqlSyntax
  type Sql92ExpressionQuantifierSyntax MysqlSyntax = MysqlSyntax
  type Sql92ExpressionCastTargetSyntax MysqlSyntax = MysqlDataTypeSyntax
  type Sql92ExpressionExtractFieldSyntax MysqlSyntax = MysqlSyntax
  type Sql92ExpressionSelectSyntax MysqlSyntax = MysqlSyntax
  addE = binOp "+"
  subE = binOp "-"
  mulE = binOp "*"
  divE = binOp "/"
  modE = binOp "%"
  orE = binOp "OR"
  andE = binOp "AND"
  likeE = binOp "LIKE"
  overlapsE = binOp "OVERLAPS"
  eqE = compOp "="
  neqE = compOp "<>"
  ltE = compOp "<"
  gtE = compOp ">"
  leE = compOp "<="
  geE = compOp ">="
  negateE = unaryOp "-"
  notE = unaryOp "NOT"
  existsE = funcall "EXISTS"
  uniqueE = funcall "UNIQUE"
  isNotNullE = postfix "IS NOT NULL"
  isNullE = postfix "IS NULL"
  isTrueE = postfix "IS TRUE"
  isFalseE = postfix "IS FALSE"
  isNotTrueE = postfix "IS NOT TRUE"
  isNotFalseE = postfix "IS NOT FALSE"
  isUnknownE = postfix "IS UNKNOWN"
  isNotUnknownE = postfix "IS NOT UNKNOWN"
  betweenE arg lo hi = bracketWrap arg <>
                        " BETWEEN " <>
                        bracketWrap lo <>
                        " AND " <>
                        bracketWrap hi
  valueE = id
  rowE = bracketWrap . fold . intersperse ", "
  fieldE = id
  subqueryE = bracketWrap
  positionE needle haystack =
    "POSITION" <> bracketWrap (bracketWrap needle <> " IN " <> bracketWrap haystack)
  nullIfE l r = "NULLIF" <> bracketWrap (l <> ", " <> r)
  absE = funcall "ABS"
  bitLengthE = funcall "BIT_LENGTH"
  charLengthE = funcall "CHAR_LENGTH"
  octetLengthE = funcall "OCTET_LENGTH"
  coalesceE = ("COALESCE" <>) . bracketWrap . fold . intersperse ", "
  extractE field from =
    "EXTRACT" <> bracketWrap (field <> " FROM " <> bracketWrap from)
  castE e to =
    "CAST" <>
      bracketWrap (bracketWrap e <> " AS " <> mysqlTypeCast to)
  caseE cases def =
    "CASE " <> foldMap go cases <> "ELSE " <> def <> " END"
    where go (cond, res) = "WHEN " <> cond <> " THEN " <> res <> " "
  currentTimestampE = "CURRENT_TIMESTAMP"
  defaultE = "DEFAULT"
  inE e es =
    bracketWrap e <> " IN " <> bracketWrap go
    where
      go = fold . intersperse ", " $ es
  trimE = funcall "TRIM"
  lowerE = funcall "LOWER"
  upperE = funcall "UPPER"

instance IsSql92AggregationSetQuantifierSyntax MysqlSyntax where
  setQuantifierDistinct = "DISTINCT"
  setQuantifierAll = "ALL"

instance IsSql92AggregationExpressionSyntax MysqlSyntax where
  type Sql92AggregationSetQuantifierSyntax MysqlSyntax = MysqlSyntax
  countAllE = "COUNT(*)"
  countE = unaryAggregation "COUNT"
  avgE = unaryAggregation "AVG"
  sumE = unaryAggregation "SUM"
  minE = unaryAggregation "MIN"
  maxE = unaryAggregation "MAX"

instance IsSql92ProjectionSyntax MysqlSyntax where
  type Sql92ProjectionExpressionSyntax MysqlSyntax = MysqlSyntax
  projExprs = fold . intersperse ", " . fmap go
    where
      go :: (MysqlSyntax, Maybe Text) -> MysqlSyntax
      go (expr, name) = expr <> foldMap go2 name
      go2 :: Text -> MysqlSyntax
      go2 = (" AS " <>) . backtickWrap . textSyntax

instance IsSql92TableNameSyntax MysqlTableNameSyntax where
  tableName = MysqlTableNameSyntax

instance IsSql92TableSourceSyntax MysqlSyntax where
  type Sql92TableSourceTableNameSyntax MysqlSyntax = MysqlTableNameSyntax
  type Sql92TableSourceSelectSyntax MysqlSyntax = MysqlSyntax
  type Sql92TableSourceExpressionSyntax MysqlSyntax = MysqlSyntax
  tableNamed = intoTableName
  tableFromSubSelect = bracketWrap
  tableFromValues =
    bracketWrap . fold . intersperse " UNION " . fmap (\vals -> "SELECT " <> go vals)
    where
      go :: [MysqlSyntax] -> MysqlSyntax
      go = fold . intersperse ", " . fmap bracketWrap

instance IsSql92FromSyntax MysqlSyntax where
  type Sql92FromTableSourceSyntax MysqlSyntax = MysqlSyntax
  type Sql92FromExpressionSyntax MysqlSyntax = MysqlSyntax
  fromTable tableE = \case
    Nothing -> tableE
    Just (name, mCols) ->
      tableE <>
      " AS " <>
      (backtickWrap . textSyntax $ name) <>
      foldMap go mCols
    where
      go :: [Text] -> MysqlSyntax
      go =
        bracketWrap . fold . intersperse ", " . fmap (backtickWrap . textSyntax)
  innerJoin = joinOp "JOIN"
  leftJoin = joinOp "LEFT JOIN"
  rightJoin = joinOp "RIGHT JOIN"

instance IsSql92GroupingSyntax MysqlSyntax where
  type Sql92GroupingExpressionSyntax MysqlSyntax = MysqlSyntax
  groupByExpressions = fold . intersperse ", "

instance IsSql92SelectTableSyntax MysqlSyntax where
  type Sql92SelectTableSelectSyntax MysqlSyntax = MysqlSyntax
  type Sql92SelectTableExpressionSyntax MysqlSyntax = MysqlSyntax
  type Sql92SelectTableProjectionSyntax MysqlSyntax = MysqlSyntax
  type Sql92SelectTableFromSyntax MysqlSyntax = MysqlSyntax
  type Sql92SelectTableGroupingSyntax MysqlSyntax = MysqlSyntax
  type Sql92SelectTableSetQuantifierSyntax MysqlSyntax = MysqlSyntax
  selectTableStmt quant proj from wher grouping having =
    "SELECT " <>
    foldMap (<> " ") quant <>
    proj <>
    foldMap (" FROM " <>) from <>
    foldMap (" WHERE " <>) wher <>
    foldMap (" GROUP BY " <>) grouping <>
    foldMap (" HAVING " <>) having
  unionTables isAll = if isAll
    then tableOp "UNION ALL"
    else tableOp "UNION"
  intersectTables _ = tableOp "INTERSECT"
  exceptTable _ = tableOp "EXCEPT"

instance IsSql92OrderingSyntax MysqlSyntax where
  type Sql92OrderingExpressionSyntax MysqlSyntax = MysqlSyntax
  ascOrdering = (<> " ASC")
  descOrdering = (<> " DESC")

instance IsSql92SelectSyntax MysqlSyntax where
  type Sql92SelectSelectTableSyntax MysqlSyntax = MysqlSyntax
  type Sql92SelectOrderingSyntax MysqlSyntax = MysqlSyntax
  selectStmt tbl ordering limit offset =
    tbl <>
    (case ordering of
      []      -> mempty
      clauses -> " ORDER BY " <> (fold . intersperse ", " $ clauses)) <>
    case (limit, offset) of
      (Just limit', Just offset') ->
        " LIMIT " <>
        (fromString . show $ offset') <>
        ", " <>
        (fromString . show $ limit')
      (Just limit', Nothing)      -> " LIMIT " <> (fromString . show $ limit')
      _                           -> mempty


instance IsSql92InsertValuesSyntax MysqlInsertValuesSyntax where
  type Sql92InsertValuesExpressionSyntax MysqlInsertValuesSyntax = MysqlSyntax
  type Sql92InsertValuesSelectSyntax MysqlInsertValuesSyntax = MysqlSyntax
  insertSqlExpressions = FromExprs
  insertFromSql = FromSQL

instance IsSql92InsertSyntax MysqlInsertSyntax where
  type Sql92InsertValuesSyntax MysqlInsertSyntax = MysqlInsertValuesSyntax
  type Sql92InsertTableNameSyntax MysqlInsertSyntax = MysqlTableNameSyntax
  insertStmt = Insert

instance IsSql92UpdateSyntax MysqlSyntax where
  type Sql92UpdateExpressionSyntax MysqlSyntax = MysqlSyntax
  type Sql92UpdateFieldNameSyntax MysqlSyntax = MysqlSyntax
  type Sql92UpdateTableNameSyntax MysqlSyntax = MysqlTableNameSyntax
  updateStmt tblName fields wher =
    "UPDATE " <>
    intoTableName tblName <>
    (case fields of
      [] -> mempty
      _  -> " SET " <> (fold . intersperse ", " . fmap go $ fields)) <>
    foldMap (" WHERE " <>) wher
    where go (field, val) = field <> "=" <> val

instance IsSql92DeleteSyntax MysqlSyntax where
  type Sql92DeleteTableNameSyntax MysqlSyntax = MysqlTableNameSyntax
  type Sql92DeleteExpressionSyntax MysqlSyntax = MysqlSyntax
  deleteStmt tblName _ wher =
    "DELETE FROM " <>
    intoTableName tblName <>
    foldMap (" WHERE " <>) wher
  deleteSupportsAlias = const False

instance IsSql92Syntax MysqlSyntax where
  type Sql92SelectSyntax MysqlSyntax = MysqlSyntax
  type Sql92InsertSyntax MysqlSyntax = MysqlInsertSyntax
  type Sql92UpdateSyntax MysqlSyntax = MysqlSyntax
  type Sql92DeleteSyntax MysqlSyntax = MysqlSyntax
  selectCmd = id
  insertCmd (Insert tblName fields values) =
    "INSERT INTO " <>
    intoTableName tblName <>
    (bracketWrap . fold . intersperse ", " . fmap textSyntax $ fields) <>
    go values
    where
      go :: MysqlInsertValuesSyntax -> MysqlSyntax
      go = \case
        FromExprs exprs ->
          "VALUES " <>
          (fold . intersperse ", " . fmap go2 $ exprs)
        FromSQL sql -> sql
      go2 :: [MysqlSyntax] -> MysqlSyntax
      go2 = bracketWrap . fold . intersperse ", "
  updateCmd = id
  deleteCmd = id

instance IsSql99ConcatExpressionSyntax MysqlSyntax where
  concatE = \case
    [] -> mempty
    xs -> "CONCAT(" <> (fold . intersperse ", " $ xs) <> ")"

-- Helpers

quoteWrap :: (IsString s, Semigroup s) => s -> s
quoteWrap = wrap "'" "'"

backtickWrap :: (IsString s, Semigroup s) => s -> s
backtickWrap = wrap "`" "`"

bracketWrap :: (IsString s, Semigroup s) => s -> s
bracketWrap = wrap "(" ")"

wrap :: (Semigroup s) => s -> s -> s -> s
wrap lDelim rDelim x = lDelim <> x <> rDelim

binOp :: (Semigroup s, IsString s) => s -> s -> s -> s
binOp op lOp rOp = bracketWrap lOp <> " " <> op <> " " <> bracketWrap rOp

compOp :: (IsString s, Foldable t, Monoid s) => s -> t s -> s -> s -> s
compOp op quantifier lOp rOp =
  bracketWrap lOp <> " " <> op <> fold quantifier <> " " <> bracketWrap rOp

unaryOp :: (Semigroup s, IsString s) => s -> s -> s
unaryOp op arg = op <> bracketWrap arg <> " "

postfix :: (Semigroup s, IsString s) => s -> s -> s
postfix op arg = bracketWrap arg <> " " <> op

funcall :: (Semigroup s, IsString s) => s -> s -> s
funcall fn arg = fn <> bracketWrap arg

unaryAggregation :: (IsString s, Foldable t, Monoid s) => s -> t s -> s -> s
unaryAggregation fn q e = fn <> bracketWrap (foldMap (<> " ") q <> e)

joinOp :: (IsString s, Foldable t, Monoid s) =>
  s -> s -> s -> t s -> s
joinOp joinType l r mOn = l <> " " <> joinType <> " " <> r <> fold mOn

tableOp :: (Semigroup s, IsString s) => s -> s -> s -> s
tableOp op l r = l <> " " <> op <> " " <> r
