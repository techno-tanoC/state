module Main where

import Control.Monad.Trans.State

addOne :: State Int ()
addOne = modify succ

doubleEven :: State Int ()
doubleEven = do
  n <- get
  if even n
  then put $ n * 2
  else return ()

main :: IO ()
main = do
  print $ execState (addOne >> doubleEven) 5
  print $ execState (doubleEven >> addOne) 5
  print $ execState (doubleEven >> addOne) 6
