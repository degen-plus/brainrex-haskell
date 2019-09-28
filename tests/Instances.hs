{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Instances where

import BrainrexAPIExplorer.Model
import BrainrexAPIExplorer.Core

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Time as TI
import qualified Data.Vector as V

import Control.Monad
import Data.Char (isSpace)
import Data.List (sort)
import Test.QuickCheck

import ApproxEq

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary TI.Day where
  arbitrary = TI.ModifiedJulianDay . (2000 +) <$> arbitrary
  shrink = (TI.ModifiedJulianDay <$>) . shrink . TI.toModifiedJulianDay

instance Arbitrary TI.UTCTime where
  arbitrary =
    TI.UTCTime <$> arbitrary <*> (TI.secondsToDiffTime <$> choose (0, 86401))

instance Arbitrary BL.ByteString where
    arbitrary = BL.pack <$> arbitrary
    shrink xs = BL.pack <$> shrink (BL.unpack xs)

instance Arbitrary ByteArray where
    arbitrary = ByteArray <$> arbitrary
    shrink (ByteArray xs) = ByteArray <$> shrink xs

instance Arbitrary Binary where
    arbitrary = Binary <$> arbitrary
    shrink (Binary xs) = Binary <$> shrink xs

instance Arbitrary DateTime where
    arbitrary = DateTime <$> arbitrary
    shrink (DateTime xs) = DateTime <$> shrink xs

instance Arbitrary Date where
    arbitrary = Date <$> arbitrary
    shrink (Date xs) = Date <$> shrink xs

-- | A naive Arbitrary instance for A.Value:
instance Arbitrary A.Value where
  arbitrary = frequency [(3, simpleTypes), (1, arrayTypes), (1, objectTypes)]
    where
      simpleTypes :: Gen A.Value
      simpleTypes =
        frequency
          [ (1, return A.Null)
          , (2, liftM A.Bool (arbitrary :: Gen Bool))
          , (2, liftM (A.Number . fromIntegral) (arbitrary :: Gen Int))
          , (2, liftM (A.String . T.pack) (arbitrary :: Gen String))
          ]
      mapF (k, v) = (T.pack k, v)
      simpleAndArrays = frequency [(1, sized sizedArray), (4, simpleTypes)]
      arrayTypes = sized sizedArray
      objectTypes = sized sizedObject
      sizedArray n = liftM (A.Array . V.fromList) $ replicateM n simpleTypes
      sizedObject n =
        liftM (A.object . map mapF) $
        replicateM n $ (,) <$> (arbitrary :: Gen String) <*> simpleAndArrays
    
-- | Checks if a given list has no duplicates in _O(n log n)_.
hasNoDups
  :: (Ord a)
  => [a] -> Bool
hasNoDups = go Set.empty
  where
    go _ [] = True
    go s (x:xs)
      | s' <- Set.insert x s
      , Set.size s' > Set.size s = go s' xs
      | otherwise = False

instance ApproxEq TI.Day where
  (=~) = (==)

-- * Models
 
instance Arbitrary InlineResponse200 where
  arbitrary =
    InlineResponse200
      <$> arbitrary -- inlineResponse200Ename :: Maybe Text
    
instance Arbitrary InlineResponse2001 where
  arbitrary =
    InlineResponse2001
      <$> arbitrary -- inlineResponse2001Currencypair :: Maybe Text
    
instance Arbitrary InlineResponse2002 where
  arbitrary =
    InlineResponse2002
      <$> arbitrary -- inlineResponse2002Blockchain :: Maybe Text
    
instance Arbitrary InlineResponse201 where
  arbitrary =
    InlineResponse201
      <$> arbitrary -- inlineResponse201StartDate :: Maybe Date
      <*> arbitrary -- inlineResponse201EndDate :: Maybe Date
      <*> arbitrary -- inlineResponse201Open :: Maybe Float
      <*> arbitrary -- inlineResponse201Close :: Maybe Float
      <*> arbitrary -- inlineResponse201High :: Maybe Float
      <*> arbitrary -- inlineResponse201Vol :: Maybe Float
    
instance Arbitrary InlineResponse2011 where
  arbitrary =
    InlineResponse2011
      <$> arbitrary -- inlineResponse2011StartDate :: Maybe Text
      <*> arbitrary -- inlineResponse2011EndDate :: Maybe Text
      <*> arbitrary -- inlineResponse2011Open :: Maybe Text
      <*> arbitrary -- inlineResponse2011Close :: Maybe Text
      <*> arbitrary -- inlineResponse2011High :: Maybe Text
      <*> arbitrary -- inlineResponse2011Vol :: Maybe Text
    
instance Arbitrary ModelText where
  arbitrary =
    ModelText
      <$> arbitrary -- modelTextText :: Maybe Text
    
instance Arbitrary Request where
  arbitrary =
    Request
      <$> arbitrary -- requestBlockchain :: Maybe Text
      <*> arbitrary -- requestMarket :: Maybe Text
      <*> arbitrary -- requestDataFormat :: Maybe Text
      <*> arbitrary -- requestOrient :: Maybe Text
      <*> arbitrary -- requestStartDate :: Maybe Text
      <*> arbitrary -- requestEndDate :: Maybe Text
    
instance Arbitrary Request1 where
  arbitrary =
    Request1
      <$> arbitrary -- request1Exchange :: Maybe Text
      <*> arbitrary -- request1Market :: Maybe Text
      <*> arbitrary -- request1DataFormat :: Maybe Text
      <*> arbitrary -- request1Orient :: Maybe Text
      <*> arbitrary -- request1StartDate :: Maybe Text
      <*> arbitrary -- request1EndDate :: Maybe Text
    
instance Arbitrary Request2 where
  arbitrary =
    Request2
      <$> arbitrary -- request2Exchange :: Maybe Text
      <*> arbitrary -- request2Market :: Maybe Text
      <*> arbitrary -- request2DataFormat :: Maybe Text
      <*> arbitrary -- request2Orient :: Maybe Text
      <*> arbitrary -- request2CandleSize :: Maybe Text
      <*> arbitrary -- request2StartDate :: Maybe Text
      <*> arbitrary -- request2EndDate :: Maybe Text
    
instance Arbitrary Request3 where
  arbitrary =
    Request3
      <$> arbitrary -- request3Exchange :: Maybe Text
      <*> arbitrary -- request3Market :: Maybe Text
      <*> arbitrary -- request3DataFormat :: Maybe Text
      <*> arbitrary -- request3StartDate :: Maybe Text
      <*> arbitrary -- request3EndDate :: Maybe Text
    
instance Arbitrary Text1 where
  arbitrary =
    Text1
      <$> arbitrary -- text1Text :: Maybe Text
    


