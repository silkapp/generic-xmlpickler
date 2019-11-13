-- | Some utility functions for using hxt picklers.
module PickleUtils
  ( toXML
  , maybeFromXML
  , eitherFromXML
  ) where

import Control.Category
import Prelude hiding ((.))
import Text.XML.HXT.Core

-- | Convert a value to an XML string.
toXML :: XmlPickler p => p -> String
toXML = showPickled []

-- | Parse a string containing xml to a value, or `Nothing` if the
-- parse fails.
maybeFromXML :: XmlPickler a => String -> Maybe a
maybeFromXML = either (const Nothing) Just . eitherFromXML

-- | Parse a string containing xml to a value, or an error message on
-- failure.
eitherFromXML :: XmlPickler a => String -> Either String a
eitherFromXML text =
  case runLA (removeAllWhiteSpace . xread) text of
    []    -> Left "Failed to parse XML in eitherFromXML."
    [x]   -> unpickleDoc' xpickle x
    (_:_) -> Left "Multiple parses in eitherFromXML."
