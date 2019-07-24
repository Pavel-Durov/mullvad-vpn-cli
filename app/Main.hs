module Main where

import qualified Data.Map as Map
import MullvadVpn (isConnected)
import Strings (toLower)
import System.Exit (exitFailure, exitSuccess)
import System.Environment (getArgs)

countryList = [
    ("cyprus", "Cyprus"),
    ("sweden", "Sweden"),
    ("united states", "United States"),
    ("united kingdom", "United Kingdom"),
    ("us", "United States"),
    ("uk", "United Kingdom"),
    ("cy", "Cyprus")
    ] 

countryMap = Map.fromList(countryList)

validateCliParamsAndCheckConnection :: String -> IO()
validateCliParamsAndCheckConnection country = do
    case lookupResult of
        Nothing -> error $ "Invalid country parameter"
        Just c -> checkConnection c
    where 
        key = toLower(country)
        lookupResult = Map.lookup key countryMap 

checkConnection :: String -> IO()
checkConnection country = do
    connected <- isConnected(country)
    if connected
        then do 
            putStrLn "👍 You are Connected"
            exitSuccess
        else do 
            putStrLn "👎 You are NOT Connected"
            exitFailure

main :: IO ()
main = do
    args <- getArgs
    let countyCliArg = head args
    validateCliParamsAndCheckConnection(countyCliArg)
