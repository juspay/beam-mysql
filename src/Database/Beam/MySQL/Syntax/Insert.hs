{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Database.Beam.MySQL.Syntax.Insert where

import           Data.Text (Text)
import           Data.Vector (Vector, fromList)
import           Database.Beam.Backend.SQL (IsSql92InsertSyntax (..),
                                            IsSql92InsertValuesSyntax (..))
import           Database.Beam.MySQL.Syntax.Select (MySQLExpressionSyntax,
                                                    MySQLSelect,
                                                    MySQLTableNameSyntax,
                                                    TableRowExpression (TableRowExpression))
import           Database.Beam.MySQL.Syntax.Update (FieldUpdate)

-- | Representation of @VALUES@ in an @INSERT@.
--
-- @since 1.2.3.1
data MySQLInsertValuesSyntax =
  InsertSQLExpressions {-# UNPACK #-} !(Vector TableRowExpression) |
  InsertFromSQL MySQLSelect
  deriving stock (
    -- | @since 1.2.3.1
    Eq
    ,
    -- | @since 1.2.3.1
    Show)

-- | @since 1.2.3.1
instance IsSql92InsertValuesSyntax MySQLInsertValuesSyntax where
  type Sql92InsertValuesExpressionSyntax MySQLInsertValuesSyntax =
    MySQLExpressionSyntax
  type Sql92InsertValuesSelectSyntax MySQLInsertValuesSyntax =
    MySQLSelect
  {-# INLINABLE insertSqlExpressions #-}
  insertSqlExpressions :: [[MySQLExpressionSyntax]] -> MySQLInsertValuesSyntax
  insertSqlExpressions =
    InsertSQLExpressions . fromList . fmap (TableRowExpression . fromList)
  {-# INLINABLE insertFromSql #-}
  insertFromSql :: MySQLSelect -> MySQLInsertValuesSyntax
  insertFromSql = InsertFromSQL

data MySQLInsert = InsertStmt
  { tableName    :: {-# UNPACK #-} !MySQLTableNameSyntax
  , columns      :: {-# UNPACK #-} !(Vector Text)
  , insertValues :: MySQLInsertValuesSyntax
  , onConflict   :: !(Maybe MySQLInsertOnConflictAction)
  }
  deriving stock (Eq, Show)

data MySQLInsertOnConflictTarget = MySQLInsertOnConflictAnyTarget
data MySQLInsertOnConflictAction
  = IGNORE
  | UPDATE_ON_DUPLICATE_KEY !(Vector FieldUpdate)
  deriving stock (Eq, Show)

instance IsSql92InsertSyntax MySQLInsert where
  type Sql92InsertValuesSyntax MySQLInsert =
    MySQLInsertValuesSyntax
  type Sql92InsertTableNameSyntax MySQLInsert =
    MySQLTableNameSyntax
  {-# INLINABLE insertStmt #-}
  insertStmt ::
    MySQLTableNameSyntax ->
    [Text] ->
    MySQLInsertValuesSyntax ->
    MySQLInsert
  insertStmt tableName' columns' insertValues' =
    InsertStmt tableName' (fromList columns') insertValues' Nothing
