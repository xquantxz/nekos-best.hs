{-# LANGUAGE OverloadedStrings #-}

module NekosBest.API (
    getNbImage,
    getNbImages
) where 

import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode, requestHeaderFieldsTooLarge431)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Aeson ( decode', FromJSON, withObject, (.:), parseJSON)
import Data.ByteString.Lazy as L ( ByteString )
import Data.Map ( Map, findWithDefault )
import Data.Maybe ( fromMaybe )
import Data.Char (toLower)

import NekosBest.Category ( NbCategory )

data NbResult = NbResult {
    artistHref :: String,
    artistName :: String,
    sourceUrl :: String,
    url :: String
} deriving (Show)

instance FromJSON NbResult where
    parseJSON = withObject "NbResult" $ \v -> NbResult
        <$> v .: "artist_href"
        <*> v .: "artist_name"
        <*> v .: "source_url"
        <*> v .: "url"

getNbImages :: (Show i, Integral i) => NbCategory -> i -> IO [NbResult]
getNbImages c i = do
    manager <- newManager tlsManagerSettings
    let endpoint = toLower <$> show c
    request <- parseRequest $ "https://nekos.best/api/v2/" ++ endpoint ++ "?amount=" ++ show i
    response <- httpLbs request manager
    let status = statusCode $ responseStatus response
    if status == 200 then do
        let json = responseBody response
        let result = getResultsFromJson json
        
        return result
    else do
        return []
    
getNbImage :: NbCategory -> IO (Maybe NbResult)
getNbImage c = do
    result <- getNbImages c 1
    return $ case result of [] -> Nothing
                            r:_ -> Just r

getResultsFromJson :: L.ByteString -> [NbResult]
getResultsFromJson json = fromMaybe [] results
    where json' = decode' json :: Maybe (Map String [NbResult])
          results = findWithDefault [] "results" <$> json'
