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
    ("united kingdom", "United Kingdom")] 

countryMap = Map.fromList(countryList)

validateCountryCliArg :: String -> Bool
validateCountryCliArg country = 
    case lookupResult of
        Nothing -> False
        Just _ -> True
    where key = toLower(country)
          lookupResult = Map.lookup key countryMap

main :: IO ()
main = do
    args <- getArgs
    let country = head(args)
    case validateCountryCliArg(country) of
        True -> checkConnection country
        False -> error "Invalid country parameter"

checkConnection :: String -> IO()
checkConnection country = do
    connected <- isConnected(country)
    if connected
        then do 
            putStrLn "Connected"
            exitSuccess
        else do 
            putStrLn "Not Connected"
            exitFailure
