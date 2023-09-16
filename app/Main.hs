module Main (main) where

import qualified Shared as S
import qualified Challenge01 as C1
import qualified Challenge02 as C2
import qualified Challenge03 as C3
import qualified Challenge04 as C4
import qualified Challenge05 as C5
import Data.Map (fromList, (!), Map)
import System.Environment (getArgs)

challenges :: Map String S.Challenge
challenges = fromList 
  [ ("01", C1.challengeSolver)
  , ("02", C2.challengeSolver)
  , ("03", C3.challengeSolver)
  , ("04", C4.challengeSolver)
  , ("05", C5.challengeSolver)
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
