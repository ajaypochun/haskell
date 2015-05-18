/* Getting json data from external URL . Tested and working */



{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

--module JSON where
import Data.Aeson as A
import GHC.Generics
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as B
import Network.HTTP as N

-- | Type of each JSON entry in record syntax.

data Temperature = Temperature
    { date  :: String
    ,temperature   :: Int
    } deriving (Show,Generic)

-- Instances to convert our type to/from JSON.


instance FromJSON Temperature
instance ToJSON Temperature

data Temp = Temp {
      temperatures :: [Temperature]
} deriving (Show,Generic)

instance FromJSON Temp
instance ToJSON Temp

-- | URL that points to the remote JSON file


jsonURL :: String
jsonURL = "http://www.phoric.eu/temperature.json" 

-- Read the remote copy of the JSON file.


getJson :: IO (Temp)
getJson = do 
  json <- N.simpleHTTP (getRequest jsonURL) >>= (getResponseBody)
  let temps = A.decode $ (B.fromStrict (C.pack json))
  case temps of    
	Just result -> return result
	Nothing -> return (Temp {temperatures=[]} )
				
getDate :: Int -> [Temp] -> Maybe [Temperature] 
getDate temp x = undefined


main :: IO ()
main = do

-- Get JSON data and print it.

    temps <- getJson
    putStrLn $ show $ temps
  
