module REPL where

import Expr
import Parsing

import System.IO
import System.Console.Haskeline

data LState = LState { vars :: [(Name, Value)] }

initLState :: LState
initLState = LState []

-- Given a variable name and a value, return a new set of variables with
-- that name and value added.
-- If it already exists, remove the old value
updateVars :: Name -> Value -> [(Name, Value)] -> [(Name, Value)]
updateVars name val vars = dropVar name vars ++ [(name,val)]

-- Return a new set of variables with the given name removed
dropVar :: Name -> [(Name, Value)] -> [(Name, Value)]
dropVar name = filter (\(n,_) -> n/=name)



process :: LState -> Command -> IO ()
process st (Set var e)
     = do let st' = LState (updateVars var (removeMaybe (eval (vars st) e)) (vars st))
          -- st' should include the variable set to the result of evaluating e
          repl st'
process st (Print e)
     = do let st' = st
          putStrLn (show  (removeMaybe (eval (vars st') e)))
          -- Print the result of evaluation
          repl st'

-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
-- 'process' will call 'repl' when done, so the system loops.

getCommand :: IO String
getCommand =  runInputT defaultSettings input
   where
      input :: InputT IO String
      input = do
         usrinput <- getInputLine "> "
         case usrinput of
            Nothing -> return []
            Just given -> return given

repl :: LState -> IO ()
repl st = do inp <- getCommand
             case parse pCommand inp of
                  [(Quit,"")] -> putStrLn "Closing session"
                  [(cmd, "")] -> -- Must parse entire input
                          process st cmd
                  _ -> do putStrLn "Parse error"
                          repl st

replaceChars :: String -> Char -> Char -> String
replaceChars s c1 c2 = map (\c -> if c==c1 then c2 else c) s

removeMaybe :: Maybe a ->  a
removeMaybe (Just a) = a
removeMaybe Nothing = error "No value"