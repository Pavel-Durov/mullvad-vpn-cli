#!/usr/bin/env stack
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module MullvadVpn( 
      getStatus, 
      isConnected,
      isConnectedVpnMessage,
      isConnectedStatus,
      Status(..)
    ) where

import Data.Aeson
import GHC.Generics
import Strings (equalIgnoreCase)
import Data.Maybe
import Network.HTTP.Simple (parseRequest, httpJSON, getResponseBody, httpBS )
import Data.List (isInfixOf)
import Data.ByteString.Lazy.Char8 (unpack, pack)
import GHC.Generics

data Status = Status { 
                country :: String
            } deriving (Eq, Generic, Show)
            
instance ToJSON Status where
  toJSON = genericToJSON defaultOptions

instance FromJSON Status where
  parseJSON = genericParseJSON defaultOptions
  
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

baseUrl = "https://am.i.mullvad.net"

getVpnConnectionMessage :: IO(String)
getVpnConnectionMessage = getText $ baseUrl ++ "/connected"

getVpnConnectionJson :: IO(Value)
getVpnConnectionJson = getJson $ baseUrl ++ "/json"

getStatus :: IO(Status)
getStatus = do
    jsonResp <- getVpnConnectionJson
    let decoded = decode (encode jsonResp) :: Maybe Status
    case decoded of
        Nothing -> error $ "Unexpected json format: " ++ unpack(encode jsonResp)
        Just status -> return(status)

isConnectedVpnMessage :: String -> Bool
isConnectedVpnMessage = isInfixOf "You are connected to Mullvad"

isConnected :: String -> IO(Bool)
isConnected countryName = do
    status <- getStatus
    connectionMessage <- getVpnConnectionMessage
    return(isConnectedStatus status countryName connectionMessage)

isConnectedStatus :: Status -> String -> String -> Bool
isConnectedStatus status countryName connectionMsg = 
    let
      isConnectedToVpn = isConnectedVpnMessage connectionMsg
      isExpectedCountry = equalIgnoreCase countryName (country status)
    in isExpectedCountry && isConnectedToVpn