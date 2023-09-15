module Challenge04(challengeSolver) where

import Crypto.Hash
import qualified Data.ByteString.UTF8 as BS

import Shared (Challenge, Solver, mkChallenge)
import Data.List (isPrefixOf)

getRightHash :: Int -> [Char] -> [Char] -> Int
getRightHash n expectedPrefix input =
  let hashed = hash . BS.fromString . (++ show n) $ input :: Digest MD5
  in if isPrefixOf expectedPrefix . show $ hashed
    then n
    else getRightHash (n + 1) expectedPrefix input

solveChallenge1 :: Solver
solveChallenge1 = show . getRightHash 0 "00000"

solveChallenge2 :: Solver
solveChallenge2 = show . getRightHash 0 "000000"

challengeSolver :: Challenge
challengeSolver = mkChallenge "challenge-04" solveChallenge1 solveChallenge2
