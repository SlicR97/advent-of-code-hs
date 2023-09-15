module Challenge01(challengeSolver) where

import Shared (Challenge, Solver, mkChallenge)

interpret :: Int -> Char -> Int
interpret n '(' = n + 1
interpret n ')' = n - 1
interpret n _ = n

solveChallenge1 :: Solver
solveChallenge1 = show . foldl interpret 0

solveChallenge2 :: Solver
solveChallenge2 = show . length . takeWhile (>=0) . scanl interpret 0

challengeSolver :: Challenge
challengeSolver = mkChallenge "challenge-01" solveChallenge1 solveChallenge2
