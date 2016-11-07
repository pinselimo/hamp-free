--
-- Copyright (c) 2016 Simon Plakolb
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU Affero General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU Affero General Public License for more details.
--
--  You should have received a copy of the GNU Affero General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>
--

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
