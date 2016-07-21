    module Lib
    ( translateHaskell
    , loadDictionary
    ) where

import Text.Parsec
import Data.List
import Data.Char
import Data.Bimap

-- TODO
-- Better error handling
-- Clean up code


haskellFile :: Parsec String (Bimap String String) String
haskellFile = do
        result <- many spaceSeparatedWord
        return $ intercalate "" result

spaceSeparatedWord :: Parsec String (Bimap String String) String
spaceSeparatedWord = do
    ss <- many haskellDelimiter
    word <- haskellWord
    ss' <- many haskellDelimiter
    return $ ss ++ word ++ ss'

haskellDelimiter :: Parsec String (Bimap String String) Char
haskellDelimiter = oneOf "-{}+*/^&<>|=\\.!;,()[]`':@~_\"" <|> space

haskellWord :: Parsec String (Bimap String String) String
haskellWord = try dictionaryReplacement
    <|> try (many1 $ satisfy (\x -> not (isSpace x)))


dictionaryReplacement :: Parsec String (Bimap String String) String
dictionaryReplacement = do
    dict <- getState
    let longestKeysFirst = Data.List.sortOn (\x -> -1 * length x) $ Data.Bimap.keys dict
    matched <- try (choice $ Data.List.map (\x -> try $ string x) longestKeysFirst)
    (lookAhead haskellDelimiter) <|> (eof >> return '0')
    let replacement = Data.Bimap.lookup matched dict
    return (head replacement)  -- WHY????


------------------------------------------------------------------------------------------------------
-- Call the parser

parseHaskell :: Bimap String String -> String -> Either ParseError String
parseHaskell dict input = runParser haskellFile dict "(unknown)" input

translateHaskell :: Bimap String String -> String -> String
translateHaskell dict s = case (parseHaskell dict s) of
    Left err -> "Error while parsing" ++ (show err)
    Right t -> t

------------------------------------------------------------------------------------------------------
-- Dictionary constructors

makeDictList :: [String] -> [(String, String)]
makeDictList [] = []
makeDictList input = (key, value) : (value, key) : makeDictList (tail $ tail input)
    where key = head input
          value = head $ tail input

loadDictionary :: String -> Bimap String String
loadDictionary s = fromList . makeDictList $ words s










