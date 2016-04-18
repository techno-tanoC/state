module Lib where
  
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

type App e s a = ExceptT e (StateT s IO) a

runApp :: App e s a -> s -> IO (Either e a, s)
runApp app s = runStateT (runExceptT app) s

increment :: Monad m => StateT Int m ()
increment = do
  modify (+ 1)

printState :: StateT Int IO ()
printState = do
  n <- get
  lift $ print n

perhap :: Monad m => Int -> ExceptT String m Bool
perhap 0 = throwE "zero!"
perhap n = return True

logging :: String -> IO ()
logging s = putStrLn $ "log: " ++ s
