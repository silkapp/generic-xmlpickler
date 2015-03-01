# generic-xmlpickler
[![Build Status](https://travis-ci.org/silkapp/generic-xmlpickler.svg?branch=master)](https://travis-ci.org/silkapp/generic-xmlpickler)

This package allows you to automatically derive
[hxt](http://hackage.haskell.org/package/hxt) picklers (conversions to
and from xml) using GHC Generics. It has been ported from
[regular-xmlpickler](http://hackage.haskell.org/package/regular-xmlpickler)

A simple example:

```Haskell
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics
import Data.Maybe (listToMaybe)
import Generics.XmlPickler (gxpickle)
import Text.XML.HXT.Arrow.Pickle (XmlPickler (..), showPickled, unpickleDoc)
import Text.XML.HXT.Parser.XmlParsec (xread)

data User = User
  { name  :: String
  , admin :: Bool
  } deriving (Show, Generic)

instance XmlPickler User where
  xpickle = gxpickle


userString :: String
userString = showPickled [] (User "Simon" True)
-- = "<user><name>Simon</name><admin>true</admin></user>"

user :: Maybe User
user = unpickleDoc xpickle =<< listToMaybe (xread "<user><name>Simon</name><admin>true</admin></user>")
-- = Just (User {name = "Simon", admin = True})
```
