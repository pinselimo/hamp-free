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
parseJSON = parse pCmd "remote" 

pCmd :: Parser JData
pCmd = spaces *> cmd <?> "Ham Command"
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
pData = value <* spaces
  where value = choice [ JString <$> pString
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
