{-# LANGUAGE TypeFamilies #-}

module Database.Beam.MySQL.Syntax.Delete where

import           Data.Text (Text)
import           Database.Beam.Backend.SQL (IsSql92DeleteSyntax (..))
import           Database.Beam.MySQL.Syntax.Select (ExpressionAnn,
                                                    MySQLExpressionSyntax,
                                                    MySQLTableNameSyntax)

data MySQLDelete = DeleteStmt {
  ann       :: !ExpressionAnn,
  tableName :: {-# UNPACK #-} !MySQLTableNameSyntax,
  wher      :: !(Maybe MySQLExpressionSyntax)
  }
  deriving stock (Eq, Show)

instance IsSql92DeleteSyntax MySQLDelete where
  type Sql92DeleteTableNameSyntax MySQLDelete =
    MySQLTableNameSyntax
  type Sql92DeleteExpressionSyntax MySQLDelete =
    MySQLExpressionSyntax
  {-# INLINABLE deleteStmt #-}
  deleteStmt ::
    MySQLTableNameSyntax ->
    Maybe Text ->
    Maybe MySQLExpressionSyntax ->
    MySQLDelete
  deleteStmt tableName' _ wher' =
    DeleteStmt (foldMap (.ann) wher') tableName' wher'
  {-# INLINABLE deleteSupportsAlias #-}
  deleteSupportsAlias = const False