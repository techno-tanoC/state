module Lib where
  
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

import Data.Char

type App e s a = ExceptT e (StateT s IO) a

runApp :: App e s a -> s -> IO (Either e a, s)
runApp app s = runStateT (runExceptT app) s

evalApp :: App e s a -> s -> IO (Either e a)
evalApp app s = evalStateT (runExceptT app) s

incState :: Monad m => StateT Int m ()
incState = modify succ

printState :: StateT Int IO ()
printState = get >>= lift . print

doubleNonZero :: Monad m => ExceptT String (StateT Int m) ()
doubleNonZero = do
  n <- lift get
  case n of
    0 -> throwE "zero!"
    n -> lift . put $ n * 2

echo :: (MonadTrans t, Monad (t IO)) => ExceptT String (t IO) ()
echo = do
  lift . lift $ putStr "type 'haskell': "
  cs <- lift . lift $ getLine
  if cs == "haskell"
    then lift . lift $ putStrLn "yes, haskell!"
    else throwE "boo, not haskell"

tellNumber :: ExceptT String (StateT Int IO) ()
tellNumber = do
    lift . lift $ putStr "tell a number: "
    s <- lift . lift $ getLine
    if isNumbers s
      then lift . put $ read s
      else throwE "not number"
  where isNumbers = all isNumber

logging :: String -> IO ()
logging s = putStrLn $ "log: " ++ s

fool :: IO ()
fool = putStrLn "hey! You fool!!"
