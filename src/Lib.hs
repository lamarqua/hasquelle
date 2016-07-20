    module Lib
    ( translateHaskell
    , loadDictionary
    ) where

import Text.Parsec
import Data.List
import Data.Char
import Data.Bimap

-- TODO
-- Command line arguments for order of translation
-- Better error handling
-- Clean up code

data TranslationOrder = NormalOrder | ReverseOrder

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
haskellDelimiter = oneOf "-{}+*/^&<>|=\\.!;,()[]`':@~_" <|> space

haskellWord :: Parsec String (Bimap String String) String
haskellWord = try (matchingGenerator NormalOrder)
    <|> try (matchingGenerator ReverseOrder)
    <|> try (many1 $ satisfy (\x -> not (isSpace x)))

-- bimapFunctions :: TranslationOrder -> (Data.Bimap.Bimap a b -> [a], a -> Data.Bimap.Bimap a b -> m b)
bimapFunctions NormalOrder = (Data.Bimap.keys, Data.Bimap.lookup)
bimapFunctions ReverseOrder = (Data.Bimap.keysR, Data.Bimap.lookupR)

matchingGenerator :: TranslationOrder -> Parsec String (Bimap String String) String
matchingGenerator order =
    let (keys', lookup') = bimapFunctions order in
    do
    -- case order of
    --     NormalOrder -> (Data.Bimap.keys, Data.Bimap.lookup)
    --     ReverseOrder -> (Data.Bimap.keysR, Data.Bimap.lookupR)
    -- let keys = if NormalOrder then Data.Bimap.keys else Data.Bimap.keysR
    -- let lookup = if NormalOrder then Data.Bimap.lookup else Data.Bimap.lookupR
    -- let keys' = case order of
    --     NormalOrder -> Data.Bimap.keys
    --     ReverseOrder -> Data.Bimap.keysR
    dict <- getState
    let longestKeysFirst = Data.List.sortOn (\x -> -1 * length x) $ keys'  dict
    matched <- try (choice $ Data.List.map (\x -> try $ string x) longestKeysFirst)
    (lookAhead haskellDelimiter) <|> (eof >> return '0')
    let replacement = lookup' matched dict
    return (head replacement)  -- WHY????


-- keyword :: Parsec String (Bimap String String) String
-- keyword = do
--     dict <- getState
--     let longestKeysFirst = Data.List.sortOn (\x -> -1 * length x) $ Data.Bimap.keys dict
--     matched <- try (choice $ Data.List.map (\x -> try $ string x) longestKeysFirst)
--     (lookAhead haskellDelimiter) <|> (eof >> return '0')
--     let replacement = Data.Bimap.lookup matched dict
--     return (head replacement)  -- WHY????

parseHaskell :: Bimap String String -> String -> Either ParseError String
parseHaskell dict input = runParser haskellFile dict "(unknown)" input

translateHaskell :: Bimap String String -> String -> String
translateHaskell dict s = case (parseHaskell dict s) of
    Left err -> "Error while parsing" ++ (show err)
    Right t -> t

makeDictList :: [String] -> [(String, String)]
makeDictList [] = []
makeDictList input = (head input, head $ tail input) : makeDictList (tail $ tail input)

loadDictionary :: String -> Bimap String String
loadDictionary s = fromList . makeDictList $ words s