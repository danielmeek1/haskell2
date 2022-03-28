module REPL where

import Expr
import Parsing

import BinaryTree
import System.IO
import System.Console.Haskeline
    ( getInputLine, defaultSettings, runInputT, InputT )
import System.Exit ( exitSuccess )
import System.Directory ( doesFileExist )



newtype LState = LState {
                -- | variables in the program
                vars :: BTree Variable }

initLState :: LState
initLState = LState Leaf

-- Given a variable name and a value, return a new set of variables with
-- that name and value added.
-- If it already exists, remove the old value
updateVars :: Name -> Value -> BTree Variable -> BTree Variable
updateVars name val vars = insert vars (Variable (name,val))


process :: LState -> [Command] -> IO ()
process st [] = repl st
process st ((Set var Input):cs)
     = do inp <- usrIn
          process (LState (updateVars var (StrVal inp) (vars st))) cs
process st ((Set var e):cs)
     = process (LState (updateVars var (removeMaybe (eval (vars st) e)) (vars st))) cs
process st ((Print e):cs)
     = do print (removeMaybe (eval (vars st) e))
          process st cs
process st ((File f):cs)
     = do
        exists <-  doesFileExist f
        processFile st f exists
        process st cs

process st (NoCommand:cs)  = process st cs
process st (Quit:cs)       = exitSuccess

processFile :: LState -> String -> Bool -> IO()
processFile st f e | e        = do
                                    contents <- readFile f
                                    let commands = map (\l -> fst(head (parse pCommand (replaceChars l '"' '\0')) )) (lines contents) --parses contents of a file into list of commands
                                    process st commands
                   |otherwise = process st [Print (Val(Error "File does not exist"))]


-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
-- 'process' will call 'repl' when done, so the system loops.

usrIn :: IO String
usrIn =  runInputT defaultSettings input
   where
      input :: InputT IO String
      input = do
         usrinput <- getInputLine ""
         case usrinput of
            Nothing -> return []
            Just given -> return given


repl :: LState -> IO ()
repl st = do putChar '>'
             inp <- usrIn
             executeCommand st inp

executeCommand :: LState -> String -> IO()
executeCommand st inp = case parse pCommand (replaceChars inp '"' '\0') of
                              [(cmd, "")] -> -- Must parse entire input
                                    process st [cmd]
                              _ -> do putStrLn "parse error"
                                      repl st

replaceChars :: String -> Char -> Char -> String
replaceChars s c1 c2 = map (\c -> if c==c1 then c2 else c) s