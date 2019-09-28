{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Data.Typeable (Proxy(..))
import Test.Hspec
import Test.Hspec.QuickCheck

import PropMime
import Instances ()

import BrainrexAPIExplorer.Model
import BrainrexAPIExplorer.MimeTypes

main :: IO ()
main =
  hspec $ modifyMaxSize (const 5) $ do
    describe "JSON instances" $ do
      pure ()
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse200)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2001)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2002)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse201)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse2011)
      propMimeEq MimeJSON (Proxy :: Proxy ModelText)
      propMimeEq MimeJSON (Proxy :: Proxy Request)
      propMimeEq MimeJSON (Proxy :: Proxy Request1)
      propMimeEq MimeJSON (Proxy :: Proxy Request2)
      propMimeEq MimeJSON (Proxy :: Proxy Request3)
      propMimeEq MimeJSON (Proxy :: Proxy Text1)
      
