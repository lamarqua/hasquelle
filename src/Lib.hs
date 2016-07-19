    module Lib
    ( translateHaskell
    ) where

import Text.ParserCombinators.Parsec
import Data.List
import Data.Char
-- import Data.Bimap

-- translations :: String -> Bimap
-- translations

haskellFile :: GenParser Char st String
haskellFile = do
        result <- many spaceSeparatedWord
        return $ intercalate "" result

spaceSeparatedWord :: GenParser Char st String
spaceSeparatedWord = do
    word <- haskellWord
    ss <- many space
    return $ word ++ ss

haskellWord :: GenParser Char st String
haskellWord = try keyword <|> try (many1 $ satisfy (\x -> not (isSpace x)))

keyword :: GenParser Char st String
keyword = string "if" >> ((lookAhead space) <|> (eof >> return '0')) >> return "si"
    -- s <- space
    -- return $ "si" ++ [s]

parseHaskell :: String -> Either ParseError String
parseHaskell input = parse haskellFile "(unknown)" input

translateHaskell :: String -> String
translateHaskell s = case (parseHaskell s) of
    Left err -> "Error while parsing" ++ (show err)
    Right t -> t
