{-# OPTIONS -fno-warn-missing-signatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE
    EmptyDataDecls
  , OverloadedStrings
  , ScopedTypeVariables
  , TemplateHaskell
  , TypeFamilies
  #-}
module Main (main) where

import Test.Tasty
import GHC.Generics (Generic)
import Test.Tasty.HUnit
import Test.Tasty.TH
import Text.XML.HXT.Arrow.Pickle
import Text.Xml.Pickle

import Generics.XmlPickler

data SingleCons = SingleCons deriving (Show, Eq, Generic)
instance XmlPickler SingleCons where xpickle = gxpickle

bidir :: (Show a, Eq a, XmlPickler a) => String -> a -> IO ()
bidir s d = do
  eq s (toXML d)
  eq (Right d) (eitherFromXML s)
  eq (Right d) (encDec d)

encDec :: XmlPickler a => a -> Either String a
encDec = eitherFromXML . toXML

eq :: (Show a, Eq a) => a -> a -> Assertion
eq = (@=?)

case_constructorWithoutFields =
  bidir "<singleCons/>" SingleCons

data Record = Record { recordField :: Int } deriving (Show, Eq, Generic)
instance XmlPickler Record where xpickle = gxpickle
case_record =
  bidir "<record><recordField>1</recordField></record>" Record { recordField = 1 }

data RecordTwoFields = D { d1 :: Int, d2 :: String } deriving (Show, Eq, Generic)
instance XmlPickler RecordTwoFields where xpickle = gxpickle
case_recordWithFields =
  bidir "<d><d1>1</d1><d2>aap</d2></d>" D {d1 = 1, d2 = "aap"}

data E = E Int deriving (Show, Eq, Generic)
instance XmlPickler E where xpickle = gxpickle
case_constructorOneField =
  bidir "<e>1</e>" (E 1)

data F = F Int String deriving (Show, Eq, Generic)
instance XmlPickler F where xpickle = gxpickle
--case_constructorWithFields = do
--  bidir "<f>1aap</f>" (F 1 "aap")

data G = G1 Int | G2 String deriving (Show, Eq, Generic)
instance XmlPickler G where xpickle = gxpickle
case_sumConstructorsWithField = do
  bidir "<g1>1</g1>" (G1 1)
  bidir "<g2>aap</g2>" (G2 "aap")

data H = H1 { h1 :: Int } | H2 { h2 :: String } deriving (Show, Eq, Generic)
instance XmlPickler H where xpickle = gxpickle
case_sumRecord = do
  bidir "<h1><h1>1</h1></h1>" H1 { h1 = 1 }
  bidir "<h2><h2>aap</h2></h2>" H2 { h2 = "aap" }

data J = J1 { j1 :: Int, j2 :: String } | J2 deriving (Show, Eq, Generic)
instance XmlPickler J where xpickle = gxpickle
case_sumRecordConstructorWithoutFields = do
  bidir "<j1><j1>1</j1><j2>aap</j2></j1>" J1 {j1 = 1, j2 = "aap"}
  bidir  "<j2/>" J2

data L = L1 | L2 Int String deriving (Show, Eq, Generic)
instance XmlPickler L where xpickle = gxpickle
case_sumConstructorWithoutFieldsConstructorWithFields =
  bidir "<l1/>" L1
--  bidir "<l2>1aap</l2>" (L2 1 "aap")

data M = M1 | M2 Int M deriving (Show, Eq, Generic)
instance XmlPickler M where xpickle = gxpickle
case_sumConstructorWithoutFieldsConstructorWithRecursiveField = do
  let a = M1
  let b = M2 1 M1
  let c = M2 1 (M2 2 M1)
  bidir "<m1/>" a
  bidir "<m2>1<m1/></m2>" b
  bidir "<m2>1<m2>2<m1/></m2></m2>" c

data N = N1 | N2 { n1 :: Int, n2 :: N } deriving (Show, Eq, Generic)
instance XmlPickler N where xpickle = gxpickle
case_sum_constructorWithoutFields_record = do
  bidir "<n1/>" N1
  bidir "<n2><n1>1</n1><n2><n1/></n2></n2>" N2 { n1 = 1, n2 = N1 }
  bidir "<n2><n1>1</n1><n2><n2><n1>2</n1><n2><n1/></n2></n2></n2></n2>" N2 { n1 = 1, n2 = N2 { n1 = 2, n2 = N1 } }

data O = O { _o :: [Int] } deriving (Show, Eq, Generic)
instance XmlPickler O where xpickle = gxpickle
--case_recordListField =
--  bidir "<o><o>123</o></o>" O {o = [1,2,3]}

data P = P [Int] deriving (Show, Eq, Generic)
instance XmlPickler P where xpickle = gxpickle
--case_constructorListField =
--  bidir "<p>123</p>" (P [1,2,3])

data Q = Q Int Int Int deriving (Show, Eq, Generic)
instance XmlPickler Q where xpickle = gxpickle
--case_ConstructorSameTypedFields =
--  bidir "<q>123</q>" (Q 1 2 3)

data T = T { r1 :: Maybe Int } deriving (Show, Eq, Generic)
instance XmlPickler T where xpickle = gxpickle
case_RecordMaybeField = do
  bidir "<t/>" T { r1 = Nothing }
  bidir "<t><r1>1</r1></t>" T { r1 = Just 1 }

data V = V1 | V2 | V3 deriving (Show, Eq, Generic)
instance XmlPickler V where xpickle = gxpickle
case_constructorsWithoutFields = do
  bidir "<v1/>" V1
  bidir "<v2/>" V2

data W = W { underscore1_ :: Int, _underscore2 :: Int } deriving (Show, Eq, Generic)
instance XmlPickler W where xpickle = gxpickle
case_recordWithUnderscoredFields =
  bidir "<w><underscore1>1</underscore1><underscore2>2</underscore2></w>" W {underscore1_ = 1, _underscore2 = 2}

data Stat = StatA | StatB (Maybe Prog)
  deriving (Eq, Show, Generic)
data Prog = Prog { aff :: Int }
  deriving (Eq, Show, Generic)
instance XmlPickler Stat where xpickle = gxpickle
instance XmlPickler Prog where xpickle = gxpickle
case_stat = do
  let a = StatB (Just Prog { aff = 1 })
  bidir "<statB><prog><aff>1</aff></prog></statB>" a

  let b = StatB Nothing
  bidir "<statB/>" b

data X = X (Maybe Int) Int deriving (Eq, Show, Generic)
instance XmlPickler X where xpickle = gxpickle
--case_constructorWithMaybeField = do
--  let a = X (Just 1) 2
--  bidir "<x>12</x>" a
--
--  let b = X Nothing 2
--  bidir "<x>2</x>" b
--
--  eq (Left "when expecting a Int, encountered Boolean instead" :: Either String X)
--     (eitherFromXML "[true,2]")

data X1 = X1 { x1a :: Maybe Int, x1b :: Int } deriving (Eq, Show, Generic)
instance XmlPickler X1 where xpickle = gxpickle
case_recordWithMaybeField = do
  let a = X1 { x1a = Just 1, x1b = 2 }
  bidir "<x1><x1a>1</x1a><x1b>2</x1b></x1>" a

  let b = X1 Nothing 2
  bidir "<x1><x1b>2</x1b></x1>" b
  eq (Nothing :: Maybe X1)
     (maybeFromXML "{\"x1a\":true,\"x1b\":2}")

data X2 = X2 { x2 :: Maybe Int }
  deriving (Eq, Show, Generic)
instance XmlPickler X2 where xpickle = gxpickle
case_recordWithOnlyOneMaybeField =
  bidir "<x2><x2>1</x2></x2>" X2 { x2 = Just 1 }

tests :: TestTree
tests = $testGroupGenerator

main :: IO ()
main = defaultMain $ testGroup "regular-xmlpickler" [tests]
