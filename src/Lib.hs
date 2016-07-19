module Lib
    ( translateHaskell
    ) where

import Text.ParserCombinators.Parsec
import Data.List


haskellFile :: GenParser Char st String
haskellFile = do
        result <- many spaceSeparatedWord
        return $ intercalate "" result

spaceSeparatedWord :: GenParser Char st String
spaceSeparatedWord = do
    word <- keywordOrElse
    ss <- many space
    return (word ++ ss)

keywordOrElse :: GenParser Char st String
keywordOrElse = try keyword <|> try (many1 $ anyChar)

keyword :: GenParser Char st String
keyword = do
    string "if"
    ss <- many1 space
    return $ "si" ++ ss

parseHaskell :: String -> Either ParseError String
parseHaskell input = parse haskellFile "(unknown)" input

translateHaskell :: String -> String
translateHaskell s = case (parseHaskell s) of
    Left err -> "Error while parsing" ++ (show err)
    Right t -> t
