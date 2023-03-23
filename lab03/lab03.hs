import System.Environment
import System.IO

prog1 :: IO ()
prog1 = do
    args <- getArgs
    mapM_ putStrLn args

prog2 :: IO ()
prog2 = do
    putStrLn "Jaki jest Twój ulubiony język programowania?"
    ans <- getLine
    if ans == "Haskell" then return () else prog2

main = prog2
