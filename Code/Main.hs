module Main where

import Parsing
import Expr
import REPL

main :: IO ()
main = do
    repl initLState
