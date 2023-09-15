module Main (main) where

import qualified Shared as S
import Data.Map (fromList, (!), Map)
import System.Environment (getArgs)

challenges :: Map String S.Challenge
challenges = fromList
  [
  ]

main :: IO ()
main = do
  [fileName] <- getArgs
  let challenge = challenges ! fileName
  f <- readFile $ "res/" ++ S.fileName challenge
  
  let s1 = S.solveChallenge1 challenge f
  let s2 = S.solveChallenge2 challenge f

  print $ "Solution to problem 1: " ++ s1
  print $ "Solution to problem 2: " ++ s2
