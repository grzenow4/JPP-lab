import Control.Monad.Error.Class
import Data.Char(isHexDigit, digitToInt)

data ParseError = Err { location :: Int, reason :: String } deriving Show

type ParseMonad = Either ParseError

parseHexDigit :: Char -> Int -> ParseMonad Integer
parseHexDigit c pos = if isHexDigit c
    then return . toInteger $ digitToInt c
    else Left $ Err pos ("Invalid input: " ++ [c])

parseHex :: String -> ParseMonad Integer
parseHex str = helper 0 str where
    helper n []     = return n
    helper n (c:cs) = do
        digit <- parseHexDigit c (length str - length cs - 1)
        helper (16 * n + digit) cs

toString :: Integer -> ParseMonad String
toString = return . show

-- convert zamienia napis z liczbą szesnastkową na napis z liczbą dziesiętną
convert :: String -> String
convert s = str where
    (Right str) = tryParse s `catchError` printError
    tryParse s = do { n <- parseHex s; toString n }
    printError e = Right $ "Error in convert: " ++ show e
