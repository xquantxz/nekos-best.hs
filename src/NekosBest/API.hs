{-# LANGUAGE OverloadedStrings #-}

module NekosBest.API (
    getNbImage,
    getNbImages,
    randomNbImage,
    randomNbImages,
    downloadNbImage
) where

import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode, requestHeaderFieldsTooLarge431)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Aeson ( decode', FromJSON, withObject, (.:?), parseJSON)
import Data.ByteString.Lazy as L ( ByteString, writeFile )
import Data.Map ( Map, findWithDefault )
import Data.Maybe ( fromMaybe )
import Data.Char (toLower)
import System.Random (RandomGen, randomR)

import NekosBest.Category ( NbCategory, allCategories )
import NekosBest.Error (NbError(..))

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

makeHttpRequest :: String -> IO (Response L.ByteString)
makeHttpRequest url = do
    manager <- newManager tlsManagerSettings
    request <- parseRequest url
    httpLbs request manager

getNbImages :: (Show i, Integral i) => NbCategory -> i -> IO (Either NbError [NbResult])
getNbImages c i = do
    let endpoint = toLower <$> show c
    response <- makeHttpRequest $ "https://nekos.best/api/v2/" ++ endpoint ++ "?amount=" ++ show i
    let status = statusCode $ responseStatus response
    if status == 200 then do
        let json = responseBody response
        let result = getResultsFromJson json

        return $ Right result
    else do
        return $ Left (NbError "Received invalid HTTP status code")

getNbImage :: NbCategory -> IO (Either NbError NbResult)
getNbImage c = do
    result <- getNbImages c 1
    return $ fmap (\(x:_) -> x) result
getResultsFromJson :: L.ByteString -> [NbResult]
getResultsFromJson json = fromMaybe [] results
    where json' = decode' json :: Maybe (Map String [NbResult])
          results = findWithDefault [] "results" <$> json'


randomNbImage :: (RandomGen g) => g -> IO (Either NbError NbResult, g)
randomNbImage gen = do
    let (c, gen') = randomCategory gen
    res <- getNbImage c
    return (res, gen')
    where randomIndex = randomR (0, length allCategories)
          randomCategory gen = let (c, gen') = randomIndex gen
                               in (allCategories !! c, gen')

randomNbImages :: (RandomGen g, Integral i) => g -> i -> IO ([Either NbError NbResult], g)
randomNbImages gen 0 = return ([], gen)
randomNbImages gen n = do
    (res, gen') <- randomNbImage gen
    (xs, gen'') <- randomNbImages gen' (n-1)
    return $ case res of Right x -> (Right x:xs, gen'')
                         Left e -> (Left e:xs, gen'')

downloadNbImage :: NbResult -> FilePath -> IO (Maybe NbError)
downloadNbImage res path = case url res of
    Just x -> do
              response <- makeHttpRequest x
              let status = statusCode $ responseStatus response
              if status == 200 then do
                let content = responseBody response
                L.writeFile path content
                return Nothing
              else do
                return $ Just $ NbError "Received invalid HTTP status code"
    Nothing -> return $ Just $ NbError "No image url in result"
