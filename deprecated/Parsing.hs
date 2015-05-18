{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Parsing where
import Data.Aeson as A
import GHC.Generics
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as B
import Network.HTTP as N


data Temperature = Temperature
    { date  :: String
    ,temperature   :: Int
    } deriving (Show,Generic)

instance FromJSON Temperature
instance ToJSON Temperature

data Temp = Temp {
      temperatures :: [Temperature]
} deriving (Show,Generic)

instance FromJSON Temp
instance ToJSON Temp

jsonURL :: String
jsonURL = "http://www.phoric.eu/temperature.json" 

getJson :: IO (Temp)
getJson = do 
  json <- N.simpleHTTP (getRequest jsonURL) >>= (getResponseBody)
  let temps = A.decode $ (B.fromStrict (C.pack json))
  case temps of    
	Just result -> return result
	Nothing -> return (Temp {temperatures=[]} )


  
