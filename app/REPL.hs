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
data LState = LState {
                -- | variables in the program
                vars :: BTree Variable}

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
                                        process (st {vars = updateVars var (StrVal inp) (vars st)}) cs
{-
     process Set command
-}
process st ((Set var e):cs)        =    process (st {vars =  updateVars var (removeMaybe (eval (vars st) e)) (vars st)}) cs
{-
     process Print command
-}
process st ((Print e):cs)          = do print (removeMaybe (eval (vars st) e))
                                        process st cs
{-
     process file command command
-}
process st ((File f):cs)           = do exists <-  doesFileExist f
                                        processFile st f exists cs

{-
     process block statement
-}
process st ((Block as):cs)         = process st (map stringToCommand (split '}' as) ++ cs)

{-
     process if statement
-}
process st ((If e t f):cs)         = case eval (vars st) e of
                                        Just (Boolean True)      -> process st (t:cs)
                                        Just (Boolean False)     -> process st (f:cs)
                                        Just (Error e)           -> process st (Print (Val(Error e)):cs)
                                        _                        -> process st (Print (Val(Error "If statement was not given a valid boolean expression")):cs)

process st ((Repeat n c):cs)       = case eval (vars st) n of
                                        Just (IntVal i)          -> do
                                                                           processRepeat st i c cs
                                        Just (Error e)           -> process st (Print (Val(Error e)):cs)
                                        _                        -> process st (Print (Val(Error "Repeat must be given an integer number of repeats")):cs)

process st ((For (Var i) t m c):cs)  = processLoop st i t m c cs

process st ((While e c):cs)          = processWhile st e c cs

process st (For {}:cs)               = process st (Print (Val(Error "Malformed for loop, usage:\nfor(<variable>;<target>;<method>)<command block>")):cs)

process st ((ExecFunc n args) :cs)   = case eval (vars st) (Var n) of
                                        Just (Funct (Function ns fcs))   -> do
                                                                      let vs = split ',' args
                                                                      let newVars = zip ns vs
                                                                      let assignments = map (\(n,v) -> Set n  (stringToExpr v)) newVars
                                                                      process st (assignments++fcs++cs)
                                        _                        -> process st (Print (Val(Error (n++" is not a function"))):cs)

{-
     process NoCommand command (user has not provided any input)
-}
process st (NoCommand:cs)  = process st cs
{-
     process Quit command
-}
process st (Quit:cs)       = exitSuccess

-- |Converts the contents of a file to a list of commands and processes them
processFile :: LState -> String -> Bool -> [Command] -> IO()
processFile st f e cs | e         =  do
                                        contents <- readFile f
                                        let commands = map stringToCommand (lines contents) --parses contents of a file into list of commands
                                        process st (commands++cs)
                      | otherwise =   process st (Print (Val(Error ("File '" ++ f ++ "' does not exist"))):cs)

processRepeat :: LState -> Int -> Command -> [Command] -> IO ()
processRepeat st i = processLoop (st {vars = updateVars "\0\0" (IntVal 0) (vars st)}) "\0\0" (EQU (Var "\0\0") (Val (IntVal i))) (Set "\0\0" (Add (Val (IntVal 1)) (Var "\0\0")))


processLoop :: LState -> Name -> Expr -> Command -> Command -> [Command] -> IO ()
processLoop st i t m c cs = case eval (vars st) t of
                              Just (Boolean True)      -> process st cs
                              Just (Boolean False)     -> process st (c:m:For (Var i) t m c:cs)
                              Just (Error e)           -> process st (Print (Val(Error e)):cs)
                              _                        -> process st (Print (Val(Error "Loop was not given a valid boolean expression")):cs)

processWhile :: LState -> Expr -> Command -> [Command] -> IO ()
processWhile st e c cs = case eval (vars st) e of
                              Just (Boolean False)     -> process st cs
                              Just (Boolean True)      -> process st (c:While e c:cs)
                              Just (Error e)           -> process st (Print (Val(Error e)):cs)
                              _                        -> process st (Print (Val(Error "Loop was not given a valid boolean expression")):cs)




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

stringToExpr ::String -> Expr
stringToExpr s = case parse pExpr  (replaceChars s '"' '\0') of
                              [(exp, "")]    -> exp
                              _              -> Val (Error "Not a valid expression")
