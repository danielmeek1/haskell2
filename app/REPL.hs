module REPL where

import Expr
import Parsing ( parse )
import BinaryTree

import System.IO ()
import System.Console.Haskeline
    ( getInputLine, defaultSettings, runInputT, InputT )
import System.Exit ( exitSuccess )
import System.Directory ( doesFileExist )

-- |The system state
newtype LState = LState {
                -- | variables in the program
                vars :: BTree Variable }

-- |The starting state of the program
initLState :: LState
initLState = LState Leaf

{- |Given a variable name and a value, return a new set of variables with
that name and value added.
If it already exists, remove the old value -}
updateVars :: Name -> Value -> BTree Variable -> BTree Variable
updateVars name val vars = insert vars (Variable (name,val))

process :: LState -> [Command] -> IO ()
process st [] = repl st
{-
     process set command when setting to an input
-}
process st ((Set var Input):cs)    = do inp <- usrIn
                                        process (LState (updateVars var (StrVal inp) (vars st))) cs
{-
     process Set command
-}
process st ((Set var e):cs)        =    process (LState (updateVars var (removeMaybe (eval (vars st) e)) (vars st))) cs
{-
     process Print command
-}
process st ((Print e):cs)          = do print (removeMaybe (eval (vars st) e))
                                        process st cs
{-
     process file command command
-}
process st ((File f):cs)           = do exists <-  doesFileExist f
                                        processFile st f exists
                                        process st cs

{-
     process NoCommand command (user has not provided any input)
-}
process st (NoCommand:cs)  = process st cs
{-
     process Quit command
-}
process st (Quit:cs)       = exitSuccess

-- |Converts the contents of a file to a list of commands and processes them
processFile :: LState -> String -> Bool -> IO()
processFile st f e | e         =  do
                                   contents <- readFile f
                                   let commands = map (\l -> fst(head (parse pCommand (replaceChars l '"' '\0')) )) (lines contents) --parses contents of a file into list of commands
                                   process st commands
                   | otherwise =   process st [Print (Val(Error ("File '" ++ f ++ "' does not exist")))]




-- |Returns a string entered by a user
usrIn :: IO String
usrIn =  runInputT defaultSettings input
   where
      input :: InputT IO String
      input = do
         usrinput <- getInputLine ""
         case usrinput of
            Nothing -> return []
            Just given -> return given

-- |Main loop of the language - gets input and executes it
repl :: LState -> IO ()
repl st = do putChar '>'
             inp <- usrIn
             executeCommand st inp

-- |Executes a command
executeCommand :: LState -> String -> IO()
executeCommand st inp = case parse pCommand (replaceChars inp '"' '\0') of
                              [(cmd, "")] -> -- Must parse entire input
                                    process st [cmd]
                              _ -> do putStrLn "parse error"
                                      repl st

-- |Replaces all instances of a character in a string with another
replaceChars :: String -> Char -> Char -> String
replaceChars s c1 c2 = map (\c -> if c==c1 then c2 else c) s