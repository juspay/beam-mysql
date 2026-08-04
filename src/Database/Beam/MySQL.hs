{-# LANGUAGE CPP         #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Module: Database.Beam.MySQL
-- Copyright: (C) Travis Authologies 2018
--            (C) Juspay Techologies Pvt Ltd 2020-21
-- License: MIT
-- Maintainer: Koz Ross <koz.ross@retro-freedom.nz>
-- Portability: GHC only
-- Stability: Experimental
--
-- MySQL/MariaDB back-end for Beam.
module Database.Beam.MySQL
(
  -- * Core
  ColumnDecodeError (..), MySQLStatementError (..),
  MySQL, MySQLM,
  runInsertRowReturning,
  runBeamMySQL, runBeamMySQLDebug,
  -- * Debugging
  dumpInsertSQL, dumpSelectSQL, dumpUpdateSQL, dumpDeleteSQL,
  -- * Helpers
  ViaJson(..), FakeUTC(..),
  -- * Syntax types
  MySQLValueSyntax(..),
  MySQLFieldNameSyntax(..),
  MySQLInsertValuesSyntax,
  MySQLExpressionSyntax,
  MySQLUpdate(..),
  -- * Compile-time information
  ParsingMethod(..),
  parsingMethod
) where

import qualified GHC.Records
import qualified GHC.Records.Extra
import           Data.FakeUTC (FakeUTC (FakeUTC))
import           Data.ViaJson (ViaJson (ViaJson))
import           Database.Beam.MySQL.Connection
import           Database.Beam.MySQL.Extra
import           Database.Beam.MySQL.Syntax.Insert
import           Database.Beam.MySQL.Syntax.Misc
import           Database.Beam.MySQL.Syntax.Select (MySQLExpressionSyntax)
import           Database.Beam.MySQL.Syntax.Update
import           Database.Beam.MySQL.Syntax.Value

-- | Describes how the library parses fields.
--
-- @since 1.2.1.1
data ParsingMethod =
  Lenient -- ^ @since 1.2.1.1
  | Strict -- ^ @since 1.2.1.1
  deriving stock (
                  Eq -- ^ @since 1.2.1.1
                  , Show -- ^ @since 1.2.1.1
                  )

-- | Specifies what parsing method this instance of the library was compiled
-- with. The @lenient@ flag determines what this is set to.
--
-- /See also:/ @beam-mysql.cabal@, @LENIENT.md@
--
-- @since 1.2.1.1
parsingMethod :: ParsingMethod
#ifdef LENIENT
parsingMethod = Lenient
#else
parsingMethod = Strict
#endif
