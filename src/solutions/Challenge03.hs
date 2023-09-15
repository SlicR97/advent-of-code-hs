module Challenge03(challengeSolver) where

import Data.Bifunctor as BF
import Data.Set as S

import Shared (Challenge, Solver, mkChallenge)

type Location = (Int, Int)

move :: Char -> Location -> Location
move '<' = BF.first (+1)
move '>' = BF.first (+(-1))
move '^' = BF.second (+1)
move 'v' = BF.second (+(-1))
move _ = id

splitList :: ([a], [a]) -> [a] -> ([a], [a])
splitList as [] = as
splitList (as, bs) [x] = (x:as, bs)
splitList (as, bs) [x, y] = (x:as, y:bs)
splitList (as, bs) (x:y:r) = splitList (x:as, y:bs) r

solveChallenge1 :: Solver
solveChallenge1 = show . length . S.fromList . scanr move (0, 0)

tupleToList :: (a, a) -> [a]
tupleToList (a0, a1) = [a0, a1]

solveChallenge2 :: Solver
solveChallenge2 = show . length . S.fromList . concatMap (scanr move (0, 0)) . tupleToList . splitList ([], [])

challengeSolver :: Challenge
challengeSolver = mkChallenge "challenge-03" solveChallenge1 solveChallenge2
