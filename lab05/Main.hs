module Main where

import Data.Map as Map
import Exp.AbsExp
import Exp.ErrM
import Exp.LexExp
import Exp.ParExp
import Exp.PrintExp
import Interpreter

run :: [Char] -> String
run s = case pExp (myLexer s) of
    Right e -> show (interpret e Map.empty)
    Left err -> err

main :: IO ()
main = do
    interact run
    putStrLn ""
