{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , KindSignatures
  , OverlappingInstances
  , ScopedTypeVariables
  , TypeOperators
  #-}
module Generics.XmlPickler
  ( gxpickle
  , GXmlPickler (..)
  , formatElement
  , xpEither
  ) where

import Data.Char (toLower)
import Data.Text (Text, pack, unpack)
import GHC.Generics
import Generics.Deriving.ConNames (ConNames)
import Text.XML.HXT.Arrow.Pickle.Schema (scAlt)
import Text.XML.HXT.Arrow.Pickle.Xml

-- | The generic XmlPickler class. This gives generic xml picklers for
-- the functors from 'Generics.Regular'. These are usually not used
-- directly.
class GXmlPickler f where
  gxpicklef :: PU a -> PU (f a)

-- Void: Used for data types without constructors
--instance GXmlPickler I where
--  gxpicklef pu = (xpWrap (I, unI) pu) { theSchema = ElemRef "data" }

instance XmlPickler a => GXmlPickler (K1 i a) where
  gxpicklef _ = (K1, unK1) `xpWrap` xpickle

instance GXmlPickler U1 where
  gxpicklef _ = (const U1, const ()) `xpWrap` xpUnit

instance (GXmlPickler f, GXmlPickler g) => GXmlPickler (f :+: g) where
  gxpicklef f = gxpicklef f `xpSum` gxpicklef f

instance (GXmlPickler f, GXmlPickler g) => GXmlPickler (f :*: g) where
  gxpicklef f = (uncurry (:*:), \(a :*: b) -> (a, b)) `xpWrap` (gxpicklef f `xpPair` gxpicklef f)

instance GXmlPickler f => GXmlPickler (M1 D c f) where
  gxpicklef f = (M1, unM1) `xpWrap` gxpicklef f

instance (Constructor c, GXmlPickler f) => GXmlPickler (M1 C c f) where
  gxpicklef f = xpElem (formatElement $ conName (undefined :: M1 C c f p)) ((M1, unM1) `xpWrap` gxpicklef f)

instance (Selector c, GXmlPickler f) => GXmlPickler (M1 S c f) where
  gxpicklef f = optElem ((M1, unM1) `xpWrap` gxpicklef f) (undefined :: M1 S c f p)

-- | The generic pickler. Uses a tag for each constructor with the
-- lower case constructor name, and a tag for each record field with
-- the lower case field name. Most values are pickled using their own
-- 'XmlPickler' instance, and 'String's are pickled as possibly empty
-- text nodes.
gxpickle :: forall a. (Generic a, GXmlPickler (Rep a), ConNames (Rep a)) => PU a
gxpickle = (to, from) `xpWrap` gxpicklef (gxpickle :: PU a)

-- * Pickling combinators

-- | Combine two picklers into a pickler for 'Either'. While pickling,
-- check if the either is a 'Left' or 'Right' and use the appropriate
-- pickler. During unpickling, first try the first, and if it fails,
-- try the second.
xpEither :: PU a -> PU b -> PU (Either a b)
xpEither ~(PU fl tl sa) ~(PU fr tr sb) = PU
  (\x st -> case x of
              Left  y -> fl y st
              Right y -> fr y st)
  (UP $ \x -> case runUP tl x of
                -- When the first fails with error message es, try the second
                (Left (es, _), _) ->
                  case runUP tr x of
                    (Left (es', _), st)  -> (Left (es ++ "\n" ++ es', st), st)
                    (Right r, st) -> (Right (Right r), st)
                (Right r, st) -> (Right (Left r), st))
  (sa `scAlt` sb)

xpSum :: PU (f r) -> PU (g r) -> PU ((f :+: g) r)
xpSum l r = (i, o) `xpWrap` xpEither l r
  where
    i (Left  x) = L1 x
    i (Right x) = R1 x
    o (L1 x) = Left x
    o (R1 x) = Right x

-- * Boolean instance for XmlPickler.

instance XmlPickler Bool where
  xpickle = (toBool, fromBool) `xpWrapEither` xpText

toBool :: String -> Either String Bool
toBool k | k' == "yes"  = Right True
         | k' == "true" = Right True
         | k' == "on"   = Right True
  where k' = map toLower k
toBool k | k' == "no"    = Right False
         | k' == "false" = Right False
         | k' == "off"   = Right False
  where k' = map toLower k
toBool k                 = Left ("XmlPickler Bool: unexpected value: " ++ k)

fromBool :: Bool -> String
fromBool True  = "true"
fromBool False = "false"

-- * Either instance for XmlPickler.

instance (XmlPickler a, XmlPickler b) => XmlPickler (Either a b) where
  xpickle = xpEither xpickle xpickle

-- * GXmlPickler instance for String, Text and Maybes.

instance GXmlPickler (K1 i String) where
  gxpicklef _ = (K1, unK1) `xpWrap` xpText0

instance GXmlPickler (K1 i Text) where
  gxpicklef _ = (K1 . pack, unpack . unK1) `xpWrap` xpText0

instance (XmlPickler a, Selector c) => GXmlPickler (M1 S c (K1 i (Maybe a))) where
  gxpicklef _ = (M1 . K1, unK1 . unM1)
         `xpWrap` xpOption (optElem xpickle (undefined :: M1 S c f p))

instance Selector c => GXmlPickler (M1 S c (K1 i (Maybe String))) where
  gxpicklef _ = (M1 . K1, unK1 . unM1)
         `xpWrap` xpOption (optElem xpText0 (undefined :: M1 S c f p))

instance Selector c => GXmlPickler (M1 S c (K1 i (Maybe Text))) where
  gxpicklef _ = (M1 . K1 . fmap pack, fmap unpack . unK1 . unM1)
         `xpWrap` xpOption (optElem xpText0 (undefined :: M1 S c f p))

-- * Utilities

formatElement :: String -> String
formatElement = headToLower . stripLeadingAndTrailingUnderscore

headToLower :: String -> String
headToLower l = case l of
  []     -> []
  (x:xs) -> toLower x : xs

stripLeadingAndTrailingUnderscore :: String -> String
stripLeadingAndTrailingUnderscore = stripLeadingUnderscore . stripTrailingUnderscore

stripLeadingUnderscore :: String -> String
stripLeadingUnderscore s = case s of
  ('_':ls) -> ls
  ls       -> ls

stripTrailingUnderscore :: String -> String
stripTrailingUnderscore s = case s of
  ""         -> ""
  (x:'_':[]) -> [x]
  (x:xs)     -> x : stripTrailingUnderscore xs

optElem :: forall a s (t :: * -> (* -> *) -> * -> *) (f :: * -> *) b. Selector s => PU a -> t s f b -> PU a
optElem x y = case formatElement (selName y) of
  "" -> x
  n  -> n `xpElem` x
