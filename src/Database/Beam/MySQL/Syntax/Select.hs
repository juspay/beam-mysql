-- Due to RDP plugin.
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Beam.MySQL.Syntax.Select where

import           Data.Text (Text)
import           Data.Vector (Vector, fromList)
import           Database.Beam.Backend.SQL (IsSql92AggregationExpressionSyntax (..),
                                            IsSql92ExpressionSyntax (..),
                                            IsSql92FromSyntax (..),
                                            IsSql92GroupingSyntax (..),
                                            IsSql92OrderingSyntax (..),
                                            IsSql92ProjectionSyntax (..),
                                            IsSql92SelectSyntax (..),
                                            IsSql92SelectTableSyntax (..),
                                            IsSql92TableNameSyntax (..),
                                            IsSql92TableSourceSyntax (..),
                                            IsSql99ConcatExpressionSyntax (..),
                                            IsSql92AggregationIndexHintsSyntax (..))
import           Database.Beam.MySQL.Syntax.DataType (MySQLDataTypeSyntax)
import           Database.Beam.MySQL.Syntax.Misc (MySQLAggregationSetQuantifierSyntax,
                                                  MySQLExtractFieldSyntax,
                                                  MySQLFieldNameSyntax,
                                                  MySQLQuantifierSyntax)
import           Database.Beam.MySQL.Syntax.Value (MySQLValueSyntax)
import           Prelude hiding (map)

data MySQLOrderingSyntax =
  AscOrdering MySQLExpressionSyntax |
  DescOrdering MySQLExpressionSyntax
  deriving stock (Eq, Show)

instance IsSql92OrderingSyntax MySQLOrderingSyntax where
  type Sql92OrderingExpressionSyntax MySQLOrderingSyntax =
    MySQLExpressionSyntax
  {-# INLINABLE ascOrdering #-}
  ascOrdering :: MySQLExpressionSyntax -> MySQLOrderingSyntax
  ascOrdering = AscOrdering
  {-# INLINABLE descOrdering #-}
  descOrdering :: MySQLExpressionSyntax -> MySQLOrderingSyntax
  descOrdering = DescOrdering

data MySQLAggregationSetIndexHintsSyntax =
  IndexHintsForce MySQLExpressionSyntax |
  IndexHintsUse MySQLExpressionSyntax
  deriving stock (Eq, Show)

instance IsSql92AggregationIndexHintsSyntax MySQLAggregationSetIndexHintsSyntax where
  type Sql92AggregationIndexHintsSyntax MySQLAggregationSetIndexHintsSyntax =
    MySQLExpressionSyntax
  {-# INLINABLE setIndexForce #-}
  setIndexForce :: MySQLExpressionSyntax -> MySQLAggregationSetIndexHintsSyntax
  setIndexForce = IndexHintsForce
  {-# INLINABLE setIndexUse #-}
  setIndexUse :: MySQLExpressionSyntax -> MySQLAggregationSetIndexHintsSyntax
  setIndexUse = IndexHintsUse

data MySQLSelectTableSyntax =
  SelectTableStatement {
    quantifier :: !(Maybe MySQLAggregationSetQuantifierSyntax),
    indexHints :: !(Maybe Text),
    projection :: {-# UNPACK #-} !MySQLProjectionSyntax,
    from       :: !(Maybe MySQLFromSyntax),
    wher       :: !(Maybe MySQLExpressionSyntax),
    grouping   :: !(Maybe MySQLGroupingSyntax),
    having     :: !(Maybe MySQLExpressionSyntax)
    } |
  UnionTablesAll {
    lTable :: MySQLSelectTableSyntax,
    rTable :: MySQLSelectTableSyntax
    } |
  UnionTablesDistinct {
    lTable :: MySQLSelectTableSyntax,
    rTable :: MySQLSelectTableSyntax
    } |
  IntersectTables {
    lTable :: MySQLSelectTableSyntax,
    rTable :: MySQLSelectTableSyntax
    } |
  ExceptTable {
    lTable :: MySQLSelectTableSyntax,
    rTable :: MySQLSelectTableSyntax
    }
  deriving stock (Eq, Show)

instance IsSql92SelectTableSyntax MySQLSelectTableSyntax where
  type Sql92SelectTableSelectSyntax MySQLSelectTableSyntax =
    MySQLSelect
  type Sql92SelectTableExpressionSyntax MySQLSelectTableSyntax =
    MySQLExpressionSyntax
  type Sql92SelectTableProjectionSyntax MySQLSelectTableSyntax =
    MySQLProjectionSyntax
  type Sql92SelectTableFromSyntax MySQLSelectTableSyntax =
    MySQLFromSyntax
  type Sql92SelectTableGroupingSyntax MySQLSelectTableSyntax =
    MySQLGroupingSyntax
  type Sql92SelectTableSetQuantifierSyntax MySQLSelectTableSyntax =
    MySQLAggregationSetQuantifierSyntax
  type Sql92SelectTableSetIndexHintsSyntax MySQLSelectTableSyntax =
    MySQLAggregationSetIndexHintsSyntax
  {-# INLINABLE selectTableStmt #-}
  selectTableStmt ::
    Maybe MySQLAggregationSetQuantifierSyntax ->
    Maybe Text ->
    MySQLProjectionSyntax ->
    Maybe MySQLFromSyntax ->
    Maybe MySQLExpressionSyntax ->
    Maybe MySQLGroupingSyntax ->
    Maybe MySQLExpressionSyntax ->
    MySQLSelectTableSyntax
  selectTableStmt = SelectTableStatement
  {-# INLINABLE unionTables #-}
  unionTables ::
    Bool ->
    MySQLSelectTableSyntax ->
    MySQLSelectTableSyntax ->
    MySQLSelectTableSyntax
  unionTables isAll lOp rOp = if isAll
    then UnionTablesAll lOp rOp
    else UnionTablesDistinct lOp rOp
  {-# INLINABLE intersectTables #-}
  intersectTables ::
    Bool ->
    MySQLSelectTableSyntax ->
    MySQLSelectTableSyntax ->
    MySQLSelectTableSyntax
  intersectTables _ lOp rOp =
    IntersectTables lOp rOp
  {-# INLINABLE exceptTable #-}
  exceptTable ::
    Bool ->
    MySQLSelectTableSyntax ->
    MySQLSelectTableSyntax ->
    MySQLSelectTableSyntax
  exceptTable _ lOp rOp =
    ExceptTable lOp rOp

data BinOp =
  LAnd |
  LOr |
  NAdd |
  NMul |
  NSub |
  NDiv |
  NMod |
  GLike |
  GOverlaps
  deriving stock (Eq, Show)

data CompOp =
  CEq |
  CNe |
  CLe |
  CLt |
  CGe |
  CGt
  deriving stock (Eq, Show)

data PrefOp =
  LNot |
  NNegate |
  TCharLength |
  TOctetLength |
  BBitLength |
  TLower |
  TUpper |
  TTrim |
  NAbs |
  TJSONValid
  deriving stock (Eq, Show)

data PostOp =
  GIsNull |
  GIsNotNull |
  GIsTrue |
  GIsNotTrue |
  GIsFalse |
  GIsNotFalse |
  GIsUnknown |
  GIsNotUnknown
  deriving stock (Eq, Show)

data AggOp =
  Count |
  Avg |
  Sum |
  Min |
  Max
  deriving stock (Eq, Show)

data CaseBranch = CaseBranch {
  condition :: !MySQLExpressionSyntax,
  action    :: !MySQLExpressionSyntax
  }
  deriving stock (Eq, Show)

-- | Represents an SQL expression (in any context).
--
-- @since 1.2.3.1
data MySQLExpressionSyntax =
  Value !MySQLValueSyntax |
  Row {-# UNPACK #-} !(Vector MySQLExpressionSyntax) |
  Coalesce {-# UNPACK #-} !(Vector MySQLExpressionSyntax) |
  Case {
    cases       :: {-# UNPACK #-} !(Vector CaseBranch),
    defaultCase :: MySQLExpressionSyntax
    } |
  Field !MySQLFieldNameSyntax |
  BinaryOperation {
    binOp    :: !BinOp,
    lOperand :: MySQLExpressionSyntax,
    rOperand :: MySQLExpressionSyntax
    } |
  ComparisonOperation {
    compOp     :: !CompOp,
    quantifier :: !(Maybe MySQLQuantifierSyntax),
    lOperand   :: MySQLExpressionSyntax,
    rOperand   :: MySQLExpressionSyntax
    } |
  PrefixOperation {
    prefOp  :: !PrefOp,
    operand :: MySQLExpressionSyntax
    } |
  PostfixOperation {
    postOp  :: !PostOp,
    operand :: MySQLExpressionSyntax
    } |
  NullIf {
    expr   :: MySQLExpressionSyntax,
    ifNull :: MySQLExpressionSyntax
    } |
  Position {
    needle   :: MySQLExpressionSyntax,
    haystack :: MySQLExpressionSyntax
    } |
  Cast {
    expr   :: MySQLExpressionSyntax,
    target :: !MySQLDataTypeSyntax
    } |
  Extract {
    field :: !MySQLExtractFieldSyntax,
    expr  :: MySQLExpressionSyntax
    } |
  JSONExtract {
    expr :: MySQLExpressionSyntax,
    key  :: MySQLExpressionSyntax
    } |
  CurrentTimestamp |
  Default |
  In {
    expr  :: MySQLExpressionSyntax,
    exprs :: {-# UNPACK #-} !(Vector MySQLExpressionSyntax)
    } |
  Between {
    expr :: MySQLExpressionSyntax,
    lo   :: MySQLExpressionSyntax,
    hi   :: MySQLExpressionSyntax
    } |
  Exists MySQLSelect |
  Unique MySQLSelect |
  Subquery MySQLSelect |
  CountAll |
  Aggregation {
    aggregationOp :: !AggOp,
    setQuantifier :: !(Maybe MySQLAggregationSetQuantifierSyntax),
    expr          :: MySQLExpressionSyntax
    } |
  Concat {-# UNPACK #-} !(Vector MySQLExpressionSyntax) |
  -- This is needed to support runInsertRowReturning.
  LastInsertId
  deriving stock (
    -- | @since 1.2.3.1
    Eq
    ,
    -- | @since 1.2.3.1
    Show)

-- | @since 1.2.3.1
instance IsSql92ExpressionSyntax MySQLExpressionSyntax where
  type Sql92ExpressionValueSyntax MySQLExpressionSyntax =
    MySQLValueSyntax
  type Sql92ExpressionFieldNameSyntax MySQLExpressionSyntax =
    MySQLFieldNameSyntax
  type Sql92ExpressionQuantifierSyntax MySQLExpressionSyntax =
    MySQLQuantifierSyntax
  type Sql92ExpressionCastTargetSyntax MySQLExpressionSyntax =
    MySQLDataTypeSyntax
  type Sql92ExpressionExtractFieldSyntax MySQLExpressionSyntax =
    MySQLExtractFieldSyntax
  type Sql92ExpressionSelectSyntax MySQLExpressionSyntax =
    MySQLSelect
  {-# INLINABLE valueE #-}
  valueE :: MySQLValueSyntax -> MySQLExpressionSyntax
  valueE = Value
  {-# INLINABLE rowE #-}
  rowE :: [MySQLExpressionSyntax] -> MySQLExpressionSyntax
  rowE = Row . fromList
  {-# INLINABLE coalesceE #-}
  coalesceE :: [MySQLExpressionSyntax] -> MySQLExpressionSyntax
  coalesceE = Coalesce . fromList
  {-# INLINABLE caseE #-}
  caseE ::
    [(MySQLExpressionSyntax, MySQLExpressionSyntax)] ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  caseE cases' def =
    Case (fromList . fmap (uncurry CaseBranch) $ cases')
         def
  {-# INLINABLE fieldE #-}
  fieldE :: MySQLFieldNameSyntax -> MySQLExpressionSyntax
  fieldE = Field
  {-# INLINABLE andE #-}
  andE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  andE = BinaryOperation LAnd
  {-# INLINABLE orE #-}
  orE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  orE = BinaryOperation LOr
  {-# INLINABLE addE #-}
  addE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  addE = BinaryOperation NAdd
  {-# INLINABLE mulE #-}
  mulE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  mulE = BinaryOperation NMul
  {-# INLINABLE subE #-}
  subE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  subE = BinaryOperation NSub
  {-# INLINABLE divE #-}
  divE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  divE = BinaryOperation NDiv
  {-# INLINABLE likeE #-}
  likeE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  likeE = BinaryOperation GLike
  {-# INLINABLE modE #-}
  modE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  modE = BinaryOperation NMod
  {-# INLINABLE overlapsE #-}
  overlapsE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  overlapsE = BinaryOperation GOverlaps
  {-# INLINABLE nullIfE #-}
  nullIfE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  nullIfE = NullIf
  {-# INLINABLE positionE #-}
  positionE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  positionE = Position
  {-# INLINABLE eqE #-}
  eqE ::
    Maybe MySQLQuantifierSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  eqE = ComparisonOperation CEq
  {-# INLINABLE neqE #-}
  neqE ::
    Maybe MySQLQuantifierSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  neqE = ComparisonOperation CNe
  {-# INLINABLE ltE #-}
  ltE ::
    Maybe MySQLQuantifierSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  ltE = ComparisonOperation CLt
  {-# INLINABLE gtE #-}
  gtE ::
    Maybe MySQLQuantifierSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  gtE = ComparisonOperation CGt
  {-# INLINABLE leE #-}
  leE ::
    Maybe MySQLQuantifierSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  leE = ComparisonOperation CLe
  {-# INLINABLE geE #-}
  geE ::
    Maybe MySQLQuantifierSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  geE = ComparisonOperation CGe
  {-# INLINABLE castE #-}
  castE ::
    MySQLExpressionSyntax ->
    MySQLDataTypeSyntax ->
    MySQLExpressionSyntax
  castE = Cast
  {-# INLINABLE notE #-}
  notE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  notE = PrefixOperation LNot
  {-# INLINABLE negateE #-}
  negateE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  negateE = PrefixOperation NNegate
  {-# INLINABLE isNullE #-}
  isNullE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  isNullE = PostfixOperation GIsNull
  {-# INLINABLE isNotNullE #-}
  isNotNullE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  isNotNullE = PostfixOperation GIsNotNull
  {-# INLINABLE isTrueE #-}
  isTrueE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  isTrueE = PostfixOperation GIsTrue
  {-# INLINABLE isNotTrueE #-}
  isNotTrueE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  isNotTrueE = PostfixOperation GIsNotTrue
  {-# INLINABLE isFalseE #-}
  isFalseE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  isFalseE = PostfixOperation GIsFalse
  {-# INLINABLE isNotFalseE #-}
  isNotFalseE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  isNotFalseE = PostfixOperation GIsNotFalse
  {-# INLINABLE isUnknownE #-}
  isUnknownE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  isUnknownE = PostfixOperation GIsUnknown
  {-# INLINABLE isNotUnknownE #-}
  isNotUnknownE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  isNotUnknownE = PostfixOperation GIsNotUnknown
  {-# INLINABLE charLengthE #-}
  charLengthE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  charLengthE = PrefixOperation TCharLength
  {-# INLINABLE octetLengthE #-}
  octetLengthE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  octetLengthE = PrefixOperation TOctetLength
  {-# INLINABLE bitLengthE #-}
  bitLengthE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  bitLengthE = PrefixOperation BBitLength
  {-# INLINABLE lowerE #-}
  lowerE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  lowerE = PrefixOperation TLower
  {-# INLINABLE upperE #-}
  upperE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  upperE = PrefixOperation TUpper
  {-# INLINABLE trimE #-}
  trimE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  trimE = PrefixOperation TTrim
  {-# INLINABLE absE #-}
  absE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  absE = PrefixOperation NAbs
  {-# INLINABLE extractE #-}
  extractE ::
    MySQLExtractFieldSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  extractE = Extract
  {-# INLINABLE jsonValidE #-}
  jsonValidE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  jsonValidE = PrefixOperation TJSONValid
  {-# INLINABLE jsonExtractE #-}
  jsonExtractE :: MySQLExpressionSyntax -> MySQLExpressionSyntax -> MySQLExpressionSyntax
  jsonExtractE = JSONExtract
  {-# INLINABLE existsE #-}
  existsE :: MySQLSelect -> MySQLExpressionSyntax
  existsE = Exists
  {-# INLINABLE uniqueE #-}
  uniqueE :: MySQLSelect -> MySQLExpressionSyntax
  uniqueE = Unique
  {-# INLINABLE subqueryE #-}
  subqueryE :: MySQLSelect -> MySQLExpressionSyntax
  subqueryE = Subquery
  {-# INLINABLE currentTimestampE #-}
  currentTimestampE :: MySQLExpressionSyntax
  currentTimestampE = CurrentTimestamp
  {-# INLINABLE defaultE #-}
  defaultE :: MySQLExpressionSyntax
  defaultE = Default
  {-# INLINABLE inE #-}
  inE ::
    MySQLExpressionSyntax ->
    [MySQLExpressionSyntax] ->
    MySQLExpressionSyntax
  inE e = In e . fromList
  {-# INLINABLE betweenE #-}
  betweenE ::
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  betweenE = Between

instance IsSql92AggregationExpressionSyntax MySQLExpressionSyntax where
  type Sql92AggregationSetQuantifierSyntax MySQLExpressionSyntax =
    MySQLAggregationSetQuantifierSyntax
  {-# INLINABLE countAllE #-}
  countAllE :: MySQLExpressionSyntax
  countAllE = CountAll
  {-# INLINABLE countE #-}
  countE ::
    Maybe MySQLAggregationSetQuantifierSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  countE = Aggregation Count
  {-# INLINABLE avgE #-}
  avgE ::
    Maybe MySQLAggregationSetQuantifierSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  avgE = Aggregation Avg
  {-# INLINABLE sumE #-}
  sumE ::
    Maybe MySQLAggregationSetQuantifierSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  sumE = Aggregation Sum
  {-# INLINABLE minE #-}
  minE ::
    Maybe MySQLAggregationSetQuantifierSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  minE = Aggregation Min
  {-# INLINABLE maxE #-}
  maxE ::
    Maybe MySQLAggregationSetQuantifierSyntax ->
    MySQLExpressionSyntax ->
    MySQLExpressionSyntax
  maxE = Aggregation Max

instance IsSql99ConcatExpressionSyntax MySQLExpressionSyntax where
  {-# INLINABLE concatE #-}
  concatE = Concat . fromList

data Projection = Projection {
  expr  :: !MySQLExpressionSyntax,
  label :: !(Maybe Text)
  }
  deriving stock (Eq, Show)

newtype MySQLProjectionSyntax = ProjectExpressions (Vector Projection)
  deriving stock (Show, Eq)

instance IsSql92ProjectionSyntax MySQLProjectionSyntax where
  type Sql92ProjectionExpressionSyntax MySQLProjectionSyntax =
    MySQLExpressionSyntax
  {-# INLINABLE projExprs #-}
  projExprs ::
    [(MySQLExpressionSyntax, Maybe Text)] ->
    MySQLProjectionSyntax
  projExprs = ProjectExpressions . fromList . fmap (uncurry Projection)

data MySQLTableNameSyntax = TableName {
  schema :: !(Maybe Text),
  name   :: {-# UNPACK #-} !Text
  }
  deriving stock (Eq, Show)

instance IsSql92TableNameSyntax MySQLTableNameSyntax where
  {-# INLINABLE tableName #-}
  tableName :: Maybe Text -> Text -> MySQLTableNameSyntax
  tableName = TableName

newtype TableRowExpression =
  TableRowExpression (Vector MySQLExpressionSyntax)
  deriving stock (Eq, Show)

data MySQLTableSourceSyntax =
  TableNamed !MySQLTableNameSyntax |
  TableFromSubSelect MySQLSelect |
  TableFromValues {-# UNPACK #-} !(Vector TableRowExpression)
  deriving stock (Eq, Show)

instance IsSql92TableSourceSyntax MySQLTableSourceSyntax where
  type Sql92TableSourceTableNameSyntax MySQLTableSourceSyntax =
    MySQLTableNameSyntax
  type Sql92TableSourceExpressionSyntax MySQLTableSourceSyntax =
    MySQLExpressionSyntax
  type Sql92TableSourceSelectSyntax MySQLTableSourceSyntax =
    MySQLSelect
  {-# INLINABLE tableNamed #-}
  tableNamed :: MySQLTableNameSyntax -> MySQLTableSourceSyntax
  tableNamed = TableNamed
  {-# INLINABLE tableFromSubSelect #-}
  tableFromSubSelect :: MySQLSelect -> MySQLTableSourceSyntax
  tableFromSubSelect = TableFromSubSelect
  {-# INLINABLE tableFromValues #-}
  tableFromValues :: [[MySQLExpressionSyntax]] -> MySQLTableSourceSyntax
  tableFromValues =
    TableFromValues . fromList . fmap (TableRowExpression . fromList)

data TableHeader =
  Anonymous |
  TableNameOnly {-# UNPACK #-} !Text |
  TableAndColumns {
    name        :: {-# UNPACK #-} !Text,
    columnNames :: {-# UNPACK #-} !(Vector Text)
    }
  deriving stock (Eq, Show)

data MySQLFromSyntax =
  FromTable {
    tableSource :: !MySQLTableSourceSyntax,
    tableHeader :: !TableHeader
    } |
  InnerJoin {
    leftArg   :: MySQLFromSyntax,
    rightArg  :: MySQLFromSyntax,
    condition :: !(Maybe MySQLExpressionSyntax)
    } |
  LeftJoin {
    leftArg   :: MySQLFromSyntax,
    rightArg  :: MySQLFromSyntax,
    condition :: !(Maybe MySQLExpressionSyntax)
    } |
  RightJoin {
    leftArg   :: MySQLFromSyntax,
    rightArg  :: MySQLFromSyntax,
    condition :: !(Maybe MySQLExpressionSyntax)
    }
  deriving stock (Eq, Show)

instance IsSql92FromSyntax MySQLFromSyntax where
  type Sql92FromTableSourceSyntax MySQLFromSyntax =
    MySQLTableSourceSyntax
  type Sql92FromExpressionSyntax MySQLFromSyntax =
    MySQLExpressionSyntax
  {-# INLINABLE fromTable #-}
  fromTable ::
    MySQLTableSourceSyntax ->
    Maybe (Text, Maybe [Text]) ->
    MySQLFromSyntax
  fromTable tableSource' mHead = FromTable tableSource' $ case mHead of
    Nothing -> Anonymous
    Just (nam, mCols) -> case mCols of
      Nothing       -> TableNameOnly nam
      Just colNames -> TableAndColumns nam . fromList $ colNames
  {-# INLINABLE innerJoin #-}
  innerJoin ::
    MySQLFromSyntax ->
    MySQLFromSyntax ->
    Maybe MySQLExpressionSyntax ->
    MySQLFromSyntax
  innerJoin = InnerJoin
  {-# INLINABLE leftJoin #-}
  leftJoin ::
    MySQLFromSyntax ->
    MySQLFromSyntax ->
    Maybe MySQLExpressionSyntax ->
    MySQLFromSyntax
  leftJoin = LeftJoin
  {-# INLINABLE rightJoin #-}
  rightJoin ::
    MySQLFromSyntax ->
    MySQLFromSyntax ->
    Maybe MySQLExpressionSyntax ->
    MySQLFromSyntax
  rightJoin = RightJoin

newtype MySQLGroupingSyntax = GroupByExpressions (Vector MySQLExpressionSyntax)
  deriving stock (Eq, Show)

instance IsSql92GroupingSyntax MySQLGroupingSyntax where
  type Sql92GroupingExpressionSyntax MySQLGroupingSyntax =
    MySQLExpressionSyntax
  {-# INLINABLE groupByExpressions #-}
  groupByExpressions :: [MySQLExpressionSyntax] -> MySQLGroupingSyntax
  groupByExpressions = GroupByExpressions . fromList

-- And finally...
data MySQLSelect = SelectStmt {
  table     :: !MySQLSelectTableSyntax,
  orderings :: {-# UNPACK #-} !(Vector MySQLOrderingSyntax),
  limit     :: !(Maybe Integer),
  offset    :: !(Maybe Integer)
  }
  deriving stock (Eq, Show)

instance IsSql92SelectSyntax MySQLSelect where
  type Sql92SelectSelectTableSyntax MySQLSelect =
    MySQLSelectTableSyntax
  type Sql92SelectOrderingSyntax MySQLSelect =
    MySQLOrderingSyntax
  {-# INLINABLE selectStmt #-}
  selectStmt ::
    MySQLSelectTableSyntax ->
    [MySQLOrderingSyntax] ->
    Maybe Integer ->
    Maybe Integer ->
    MySQLSelect
  selectStmt table' orderings' = SelectStmt table' (fromList orderings')
