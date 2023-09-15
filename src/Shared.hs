module Shared(FileName, Solver, Challenge(..), Challenges, mkChallenge) where

type FileName = String
type Solver = String -> String

data Challenge = Challenge
  { fileName :: FileName
  , solveChallenge1 :: Solver
  , solveChallenge2 :: Solver }

mkChallenge :: FileName -> Solver -> Solver -> Challenge
mkChallenge f s1 s2 = Challenge { fileName = f, solveChallenge1 = s1, solveChallenge2 = s2 }

type Challenges = [Challenge]
