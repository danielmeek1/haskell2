module Main where

import Parsing
import Expr
import REPL
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    if null args then
        repl initLState else
            executeCommand initLState ("file \0" ++ head args ++ "\0")
