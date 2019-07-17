#!/usr/bin/env stack
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module MullvadVpn
    ( getStatus, 
      isConnected
    ) where

import Strings (equalIgnoreCase)
import Data.Maybe
import Network.HTTP.Simple (parseRequest, httpJSON, getResponseBody, httpBS )
import Data.Aeson
import GHC.Generics
import Data.List (isInfixOf)
import Data.ByteString.Lazy.Char8 (unpack, pack)

import GHC.Generics

data Status = Status { 
                country :: String
              , ip :: String
            } deriving (Eq, Generic, Show)

instance ToJSON Status where
  toJSON = genericToJSON defaultOptions

instance FromJSON Status where
  parseJSON = genericParseJSON defaultOptions

getJson url = do
    request <- parseRequest $ "GET " ++ url
    response <- httpJSON request
    return(getResponseBody response)

getText url = do
    request <- parseRequest $ "GET " ++ url
    response <- httpBS request
    return(show (getResponseBody response))

getVpnConnectionMessage = getText "https://am.i.mullvad.net/connected"

getVpnConnectionJson = getJson "https://am.i.mullvad.net/json"

getStatus :: IO(Status)
getStatus = do
    jsonResp <- getVpnConnectionJson
    let decoded = decode ( encode (jsonResp :: Value)) :: Maybe Status
    case decoded of
        Nothing -> error $ "Unexpected json format: " ++ unpack(encode (jsonResp :: Value))
        Just status -> return(status)

isConnectedVpnMessage :: String -> Bool
isConnectedVpnMessage = isInfixOf "You are connected to Mullvad"

isConnected :: String -> IO(Bool)
isConnected countryName = do
    status <- getStatus
    let connectionCountry = country status
    let isExpectedCountry = equalIgnoreCase countryName connectionCountry
    isConnectedToVpn <- fmap isConnectedVpnMessage getVpnConnectionMessage
    return(isExpectedCountry && isConnectedToVpn)
