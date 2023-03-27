module Main where

import Tiny.AbsTiny
import Tiny.ErrM
import Tiny.LexTiny
import Tiny.ParTiny
import Tiny.PrintTiny
import Interpreter

go :: [Char] -> String
go s = case pProgram (myLexer s) of
    Ok e -> show (run e)
    Left err -> err

main :: IO ()
main = do
    interact go
    putStrLn ""
