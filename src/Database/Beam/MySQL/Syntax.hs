{-# LANGUAGE TypeFamilies #-}

module Database.Beam.MySQL.Syntax where

import qualified GHC.Records
import qualified GHC.Records.Extra
import           Database.Beam.Backend.SQL (IsSql92Syntax (..))
import           Database.Beam.MySQL.Syntax.Delete (MySQLDelete)
import           Database.Beam.MySQL.Syntax.Insert (MySQLInsert)
import           Database.Beam.MySQL.Syntax.Select (MySQLSelect)
import           Database.Beam.MySQL.Syntax.Update (MySQLUpdate)

data MySQLSyntax =
  ASelect MySQLSelect |
  AnInsert MySQLInsert |
  AnUpdate MySQLUpdate |
  ADelete MySQLDelete
  deriving stock (Eq, Show)

instance IsSql92Syntax MySQLSyntax where
  type Sql92SelectSyntax MySQLSyntax = MySQLSelect
  type Sql92UpdateSyntax MySQLSyntax = MySQLUpdate
  type Sql92DeleteSyntax MySQLSyntax = MySQLDelete
  type Sql92InsertSyntax MySQLSyntax = MySQLInsert
  {-# INLINABLE selectCmd #-}
  selectCmd = ASelect
  {-# INLINABLE insertCmd #-}
  insertCmd = AnInsert
  {-# INLINABLE updateCmd #-}
  updateCmd = AnUpdate
  {-# INLINABLE deleteCmd #-}
  deleteCmd = ADelete
