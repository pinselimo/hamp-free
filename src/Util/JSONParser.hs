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

{-# LANGUAGE FlexibleContexts #-}
module Util.JSONParser 
    (parseJSON)
    where

import Util.JSON

import Text.ParserCombinators.Parsec hiding (Parser)
import Control.Applicative (empty, (<$>), (<$), (<*>),(<*),(*>))
import Numeric (readDec, readHex)

type Parser a = CharParser () a

parseJSON :: String -> Either ParseError JData
parseJSON = parse command "remote" 

command :: Parser JData
command = spaces *> cmd <?> "Ham Command"
     where cmd = JObject <$> pObject --comands are represented by objects
    
closure :: Char -> Parser a -> Char -> Char -> Parser [a]
closure l p s r = between (char l <* spaces) (char r) parser
   where parser = (p <* spaces) `sepBy` (char s <* spaces)

pArray :: Parser [JData]
pArray = closure '[' pData ',' ']'

pObject :: Parser [(String,JData)]
pObject = closure '{' fields ',' '}'
    where fields = (,) <$> (pString <* char ':' <* spaces)  <*> pData


pData :: Parser JData
pData = spaces *> choice [ JString <$> pString
                         , JNumber <$> pNumber
                         , JObject <$> pObject
                         , JArray  <$> pArray
                         , JBool   <$> pBool
                         , JNull   <$ string "null"]
              <?> "Ham command value"

pBool :: Parser Bool
pBool = True  <$ string "true"
    <|> False <$ string "false" 

pNumber :: Parser Int
pNumber = do s <- getInput
             case readDec s of
                [(n,s')] -> n <$ setInput s'
                _        -> empty

pString :: Parser String
pString = between (char '\"') (char '\"') (many jchar)
    where jchar = char '\\' *> (pEscape <|> pUnicode)
              <|> satisfy (`notElem` "\"\\")

pEscape = choice (zipWith decode "bnfrt\\\"/" "\b\n\f\r\t\\\"/")
    where decode c r = r <$ char c

pUnicode :: Parser Char
pUnicode = char 'u' *> (decode <$> count 4 hexDigit)
    where decode x = toEnum code
                   where ((code,_):_) = readHex x

example :: String
example = "{\n" ++
 "  \"Herausgeber\":\"Xema\",\n" ++
 "  \"Nummer\": \"1234-5678-9012-3456\",\n" ++
 "  \"Deckung\": 2 ,\n" ++
 "  \"Waehrung\": \"EURO\",\n" ++
 "  \"Inhaber\": \n" ++
 "  {\n" ++
 "    \"Name\": \"Mustermann\",\n" ++
 "    \"Vorname\": \"Max\",\n" ++
 "    \"maennlich\": true,\n" ++
 "    \"Hobbys\": [ \"Reiten\", \"Golfen\", \"Lesen\" ],\n" ++
 "    \"Alter\": 42,\n" ++
 "    \"Kinder\": [],\n" ++
 "    \"Partner\": null\n" ++
 "  }\n" ++
 "}"
