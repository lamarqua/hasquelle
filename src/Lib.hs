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
    word <- haskellWord
    ss <- many space
    return $ word ++ ss

haskellWord :: Parsec String (Bimap String String) String
haskellWord = try keyword' <|> try (many1 $ satisfy (\x -> not (isSpace x)))

keyword' :: Parsec String (Bimap String String) String
keyword' = do
    dict <- getState
    matched <- try (choice $ Data.List.map (\x -> string x) (Data.Bimap.keys dict))
    (lookAhead space) <|> (eof >> return '0')
    return (Data.Bimap.lookup matched dict)
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