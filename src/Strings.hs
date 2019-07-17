module Strings
    ( toLower,
      equalIgnoreCase
    ) where


import qualified Data.Char as Ch

toLower :: String -> String
toLower = map Ch.toLower

equalIgnoreCase :: String -> String -> Bool
equalIgnoreCase a b = toLower(a) == toLower(b)    