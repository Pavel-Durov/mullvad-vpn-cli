module Network (
        getJson,
        getText
    ) where

import Data.Aeson (Value)
import Network.HTTP.Simple (parseRequest, httpJSON, getResponseBody, httpBS)

getJson :: String -> IO(Value) 
getJson url = do
    request <- parseRequest $ "GET " ++ url
    response <- httpJSON request
    return(getResponseBody $ response :: Value)
    
getText :: String -> IO(String)    
getText url = do
    request <- parseRequest $ "GET " ++ url
    response <- httpBS request
    return(show (getResponseBody response))
