{-| Module for Unicode text handling.

The database has a Latin1 encoding, but we want to accept any unicode data.
So instead of erasing unicode codepoints >255, we re-encode text as
UTF8-encoded Latin1 text. In particular:

  encodeText = decodeLatin1 . encodeUtf8

This works for all text, as latin1 can decode all bytes into unicode
codepoints. We then decode using the converse method:

  decodeText = decodeUtf8 . encodeLatin1

However, to complicate matters further, we also have to deal with data
that is old, or comes from other production systems, and so may be:

  * valid Latin-1
  * other

In our case, <other> will be treated as latin1 text.

The full sequence of transformations, including mysql-haskell should
now be for WRITE:

  beam-mysql:    decodeLatin1 . encodeUtf8
  mysql-haskell: encodeLatin1

Which gives `encodeLatin1 . decodeLatin1 . encodeUtf8 = encodeUtf8`.

For READ we do the following:

  mysql-haskell: decodeLatin1
  beam-mysql:    decodeUtf8 . encodeLatin1

Which gives `decodeUtf8 . encodeLatin1 . decodeLatin1 = decodeUtf8`.
-}

module Database.Beam.MySQL.TextHandling
  (
    encodeText, decodeText
  )
where

import qualified GHC.Records
import qualified GHC.Records.Extra
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import           Data.Text (Text, unpack)
import           Data.Text.Encoding (decodeLatin1, decodeUtf8', encodeUtf8)

-- | Re-encode unicode at UTF8-encoded latin1 text.
--
-- NOTE: Use this function to decode any text coming from mysql-haskell.
--
encodeText :: Text -> Text
encodeText = decodeLatin1 . encodeUtf8

-- | Decode UTF-8 text that was possibly re-encoded as latin1 text.
--   If it is not re-encoded text, return the original.
--
-- NOTE: Use this function to decode any text coming from mysql-haskell.
--
decodeText :: Text -> Text
decodeText text = case decodeUtf8' . encodeLatin1 $ text of
  Left _      -> text
  Right text' -> text'

-----------------------------------------------------------------------------
-- Helpers
-----------------------------------------------------------------------------

-- Reverse-engineered from description of decodeLatin1
encodeLatin1 :: Text -> ByteString
encodeLatin1 = Char8.pack . unpack
