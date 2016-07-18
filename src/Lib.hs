module Lib
    ( translateHaskell
    ) where

import Text.ParserCombinators.Parsec
import Data.List

haskellFile :: GenParser Char st String
haskellFile = do
        result <- many anywordOrSpace --(keyword <* space)
        eof
        return $ intercalate "" result



-- anywordOrSpace :: GenParser Char st String
-- anywordOrSpace =
--     (try keyword)
--     <|> (try notKeyword)
--     <|> do
--             s <- space
--             ss <- many space
--             return $ s : ss

-- spaceParser :: GenParser Char st String
-- spaceParser = do
--         foundSpaces <- many space
--         return $ concat foundSpaces

-- hwords :: GenParser Char st String
-- hwords = do
--     first <- keyword -- <|> notKeyword --followed by a space
--     whiteSpace <- many space --- FIX: make many spaces
--     next <- hwords
--     return (first ++ whiteSpace ++ next)

-- remainingWords :: GenParser Char st String
-- remainingWords = do


keyword :: GenParser Char st String
keyword = try $ string "if" >> return "si"

notKeyword :: GenParser Char st String
notKeyword = do
            c <- anyChar
            cs <- many anyChar
            return $ c : cs

parseHaskell :: String -> Either ParseError String
parseHaskell input = parse haskellFile "(unknown)" input

translateHaskell :: String -> String
translateHaskell s = case (parseHaskell s) of
    Left err -> "Error while parsing" ++ (show err)
    Right t -> t
