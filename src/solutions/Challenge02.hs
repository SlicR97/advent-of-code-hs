module Challenge02(challengeSolver) where

import Text.Regex.TDFA

import Shared (Challenge, Solver, mkChallenge)

type Dimensions = (Int, Int, Int)

surfaceArea :: Dimensions -> Int
surfaceArea (x, y, z) = 2*x*y + 2*x*z + 2*y*z

smallestArea :: Dimensions -> Int
smallestArea (x, y, z) = minimum [x*y, x*z, y*z]

smallestPerimeter :: Dimensions -> Int
smallestPerimeter (x, y, z) = 2 * minimum [x+y, x+z, y+z]

volume :: Dimensions -> Int
volume (x, y, z) = x*y*z

parseLine :: String -> Dimensions
parseLine s =
  let regex = "^([0-9]+)x([0-9]+)x([0-9]+)$"
      groups = s =~ regex :: [[String]]
      m = head groups
  in (read $ m !! 1, read $ m !! 2, read $ m !! 3)

solveChallenge1 :: Solver
solveChallenge1 = show . sum . map (calc . parseLine) . lines
  where calc dim = surfaceArea dim + smallestArea dim

solveChallenge2 :: Solver
solveChallenge2 = show . sum . map (calc . parseLine) . lines
  where calc dim = smallestPerimeter dim + volume dim

challengeSolver :: Challenge
challengeSolver = mkChallenge "challenge-02" solveChallenge1 solveChallenge2
