{-# LANGUAGE OverloadedStrings #-}

module NekosBest.API (
    getNbImage,
    getNbImages,
    randomNbImage,
    randomNbImages
) where 

import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode, requestHeaderFieldsTooLarge431)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Aeson ( decode', FromJSON, withObject, (.:?), parseJSON)
import Data.ByteString.Lazy as L ( ByteString )
import Data.Map ( Map, findWithDefault )
import Data.Maybe ( fromMaybe )
import Data.Char (toLower)
import System.Random (RandomGen, randomR)

import NekosBest.Category ( NbCategory, allCategories )

data NbResult = NbResult {
    artistHref :: Maybe String,
    artistName :: Maybe String,
    sourceUrl :: Maybe String,
    animeName :: Maybe String,
    url :: Maybe String
} deriving (Show)

instance FromJSON NbResult where
    parseJSON = withObject "NbResult" $ \v -> NbResult
        <$> v .:? "artist_href"
        <*> v .:? "artist_name"
        <*> v .:? "source_url"
        <*> v .:? "anime_name"
        <*> v .:? "url"

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


randomNbImage :: (RandomGen g) => g -> IO (Maybe NbResult, g)
randomNbImage gen = do
    let (c, gen') = randomCategory gen
    print c
    res <- getNbImage c
    return (res, gen')
    where randomIndex = randomR (0, length allCategories)
          randomCategory gen = let (c, gen') = randomIndex gen
                               in (allCategories !! c, gen')

randomNbImages :: (RandomGen g, Integral i) => g -> i -> IO ([NbResult], g)
randomNbImages gen 0 = return ([], gen)
randomNbImages gen n = do
    (res, gen') <- randomNbImage gen
    (xs, gen'') <- randomNbImages gen' (n-1)
    return $ case res of Just x -> (x:xs, gen'')
                         Nothing -> (xs, gen'')

