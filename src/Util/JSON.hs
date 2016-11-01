module Util.JSON (JData(..), serialize, getString, getNum, getBool)
    where

import Data.Char (isDigit, toLower)

data JData = JObject [(String,JData)]
           | JArray  [JData]
           | JString String
           | JBool   Bool
           | JNumber Int
           | JNull

-- Show instance produces valid JSON
instance Show JData where
    show (JObject d) = '{': (foldr (\s r -> case r of
                                                [] -> s ++ r
                                                _  -> s ++ ',':r) "" $
                     map showD  d) 
                     ++ "}"
    show (JArray  d) = show d
    show (JString s) = show s
    show (JBool   b) 
        | b          = "true"
        | otherwise  = "false"
    show (JNumber n) = show n
    show JNull       = "null"

showD :: (String,JData) -> String
showD (k,d) = show k ++ ':':show d

serialize :: JData -> String
serialize = show

getString :: JData -> Maybe String
getString (JString s) = Just s
getString _           = Nothing

getNum :: JData -> Maybe Int
getNum (JNumber i) = Just i
getNum _           = Nothing

getBool :: JData -> Maybe Bool
getBool (JBool b) = Just b
getBool _         = Nothing
