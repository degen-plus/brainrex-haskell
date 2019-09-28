{-
   Brainrex API Explorer

   Welcome to the Brainrex API explorer, we make analytics tools for crypto and blockchain. Our currently propiertary models offer sentiment analysis, market making, blockchain monitoring and face-id verification. This AI models can be consumed from this API. We also offer integrations to open data and propietary data providers, as well as free test data we collect. There is a collection of data transformation tools. Join our Telegram group to get the latest news and ask questions [https://t.me/brainrex, #brainrex](https://t.me/brainrex). More about Brainrex at [https://brainrex.com](http://brainrex.com). Full Documentation can be found at [https://brainrexapi.github.io/docs](https://brainrexapi.github.io/docs)

   OpenAPI spec version: 2.0
   Brainrex API Explorer API version: 0.1.1
   Contact: support@brainrex.com
   Generated by Swagger Codegen (https://github.com/swagger-api/swagger-codegen.git)
-}

{-|
Module : BrainrexAPIExplorer.API.Crypto
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module BrainrexAPIExplorer.API.Crypto where

import BrainrexAPIExplorer.Core
import BrainrexAPIExplorer.MimeTypes
import BrainrexAPIExplorer.Model as M

import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Data as P (Typeable, TypeRep, typeOf, typeRep)
import qualified Data.Foldable as P
import qualified Data.Map as Map
import qualified Data.Maybe as P
import qualified Data.Proxy as P (Proxy(..))
import qualified Data.Set as Set
import qualified Data.String as P
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Time as TI
import qualified Network.HTTP.Client.MultipartFormData as NH
import qualified Network.HTTP.Media as ME
import qualified Network.HTTP.Types as NH
import qualified Web.FormUrlEncoded as WH
import qualified Web.HttpApiData as WH

import Data.Text (Text)
import GHC.Base ((<|>))

import Prelude ((==),(/=),($), (.),(<$>),(<*>),(>>=),Maybe(..),Bool(..),Char,Double,FilePath,Float,Int,Integer,String,fmap,undefined,mempty,maybe,pure,Monad,Applicative,Functor)
import qualified Prelude as P

-- * Operations


-- ** Crypto

-- *** exchangesDownloadCandles

-- | @POST \/download_candles@
-- 
-- Downloads candle format market data
-- 
-- Returns a list of candle data from specified market and data range
-- 
exchangesDownloadCandles 
  :: (Consumes ExchangesDownloadCandles MimeJSON, MimeRender MimeJSON Request2)
  => Request2 -- ^ "request" -  Person to create
  -> BrainrexAPIExplorerRequest ExchangesDownloadCandles MimeJSON InlineResponse201 MimeJSON
exchangesDownloadCandles request =
  _mkRequest "POST" ["/download_candles"]
    `setBodyParam` request

data ExchangesDownloadCandles 

-- | /Body Param/ "request" - Person to create
instance HasBodyParam ExchangesDownloadCandles Request2 

-- | @application/json@
instance Consumes ExchangesDownloadCandles MimeJSON

-- | @application/json@
instance Produces ExchangesDownloadCandles MimeJSON


-- *** exchangesList

-- | @GET \/markets@
-- 
-- The markets data structure supported by the Brainrex Market API
-- 
-- Read the list of supported markets ( currency pairs ) in each exchange
-- 
exchangesList 
  :: BrainrexAPIExplorerRequest ExchangesList MimeNoContent [A.Value] MimeJSON
exchangesList =
  _mkRequest "GET" ["/markets"]

data ExchangesList  

-- | @application/json@
instance Consumes ExchangesList MimeJSON

-- | @application/json@
instance Produces ExchangesList MimeJSON


-- *** exchangesMarketmaker

-- | @POST \/market_making@
-- 
-- Market Making as a Service API.
-- 
-- Our AI will trade at the risk level you want, you need to provide your credential to the exchange you are trading at.
-- 
exchangesMarketmaker 
  :: (Consumes ExchangesMarketmaker MimeJSON, MimeRender MimeJSON Request3)
  => Request3 -- ^ "request" -  Name of exchange and currency pair you want to provide liquidity
  -> BrainrexAPIExplorerRequest ExchangesMarketmaker MimeJSON InlineResponse2011 MimeJSON
exchangesMarketmaker request =
  _mkRequest "POST" ["/market_making"]
    `setBodyParam` request

data ExchangesMarketmaker 

-- | /Body Param/ "request" - Name of exchange and currency pair you want to provide liquidity
instance HasBodyParam ExchangesMarketmaker Request3 

-- | @application/json@
instance Consumes ExchangesMarketmaker MimeJSON

-- | @application/json@
instance Produces ExchangesMarketmaker MimeJSON


-- *** exchangesRead

-- | @GET \/exchanges@
-- 
-- The exchanges data structure supported by the Brainrex API
-- 
-- Read the list of supported exchanges in the Market Data API
-- 
exchangesRead 
  :: BrainrexAPIExplorerRequest ExchangesRead MimeNoContent [A.Value] MimeJSON
exchangesRead =
  _mkRequest "GET" ["/exchanges"]

data ExchangesRead  

-- | @application/json@
instance Consumes ExchangesRead MimeJSON

-- | @application/json@
instance Produces ExchangesRead MimeJSON


-- *** exchangesTickerDataDownload

-- | @POST \/download_ticker@
-- 
-- Download raw ticker data from major crypto markets
-- 
-- Downloads specified asset class and market and time frame. Of our raw ticker data format
-- 
exchangesTickerDataDownload 
  :: (Consumes ExchangesTickerDataDownload MimeJSON, MimeRender MimeJSON Request1)
  => Request1 -- ^ "request" -  Person to create
  -> BrainrexAPIExplorerRequest ExchangesTickerDataDownload MimeJSON InlineResponse201 MimeJSON
exchangesTickerDataDownload request =
  _mkRequest "POST" ["/download_ticker"]
    `setBodyParam` request

data ExchangesTickerDataDownload 

-- | /Body Param/ "request" - Person to create
instance HasBodyParam ExchangesTickerDataDownload Request1 

-- | @application/json@
instance Consumes ExchangesTickerDataDownload MimeJSON

-- | @application/json@
instance Produces ExchangesTickerDataDownload MimeJSON
