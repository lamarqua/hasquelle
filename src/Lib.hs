    module Lib
    ( translateHaskell
    , loadDictionary
    ) where

import Text.Parsec
import Data.List
import Data.Char
import Data.Bimap

-- translations :: String -> Bimap
-- translations

haskellFile :: Parsec String (Bimap String String) String
haskellFile = do
        result <- many spaceSeparatedWord
        return $ intercalate "" result

spaceSeparatedWord :: Parsec String (Bimap String String) String
spaceSeparatedWord = do
    ss <- many space
    word <- haskellWord
    ss' <- many space
    return $ ss ++ word ++ ss'

haskellDelimiter :: Parsec String (Bimap String String) Char
haskellDelimiter = oneOf "-{}+*/^&<>|=\\.!;,()[]`':@~_" <|> space

haskellWord :: Parsec String (Bimap String String) String
haskellWord = try keyword' <|> try (many1 $ satisfy (\x -> not (isSpace x)))

keyword' :: Parsec String (Bimap String String) String
keyword' = do
    dict <- getState
    let longestKeysFirst = Data.List.sortOn (\x -> -1 * length x) $ Data.Bimap.keys dict
    matched <- try (choice $ Data.List.map (\x -> try $ string x) longestKeysFirst)
    (lookAhead space) <|> (eof >> return '0')
    let replacement = Data.Bimap.lookup matched dict
    return (head replacement)  -- WHY????
    -- s <- space
    -- return $ "si" ++ [s]

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