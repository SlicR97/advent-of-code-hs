module Challenge05(challengeSolver) where

import Shared (Challenge, Solver, mkChallenge)
import Data.List (isPrefixOf, group)

containsThreeVowels :: String -> Bool
containsThreeVowels = (>=3) . length . filter (`elem` "aeiou")

containsLetterTwiceInARow :: String -> Bool
containsLetterTwiceInARow = any ((>= 2) . length) . group

everyN :: Int -> [a] -> [[a]]
everyN _ [] = []
everyN n as
  | n > length as = []
  | otherwise = take n as : everyN n (tail as)

containsForbiddenStrings :: String -> Bool
containsForbiddenStrings = any isForbiddenSequence . everyN 2
  where isForbiddenSequence s = s `elem` ["ab", "cd", "pq", "xy"]

containsPairOfTwoLetters :: String -> Bool
containsPairOfTwoLetters [] = False
containsPairOfTwoLetters [_] = False
containsPairOfTwoLetters [_, _] = False
containsPairOfTwoLetters [_, _, _] = False
containsPairOfTwoLetters (x:y:r) =
  isInString [x, y] r || containsPairOfTwoLetters (y:r)
  where isInString _ [] = False
        isInString _ [_] = False
        isInString sub str = (sub `isPrefixOf` str) || isInString sub (tail str)

containsRepeatingLetterWithLetterBetween :: String -> Bool
containsRepeatingLetterWithLetterBetween s = any pre $ everyN 3 s
  where pre [a0, b, a1] = (a0 == a1) && (a0 /= b)
        pre _ = False

isNiceString1 :: String -> Bool
isNiceString1 s =
  containsThreeVowels s &&
  containsLetterTwiceInARow s &&
  not (containsForbiddenStrings s)

isNiceString2 :: String -> Bool
isNiceString2 s =
  containsPairOfTwoLetters s &&
  containsRepeatingLetterWithLetterBetween s

solveChallenge1 :: Solver
solveChallenge1 = show . length . filter isNiceString1 . lines

solveChallenge2 :: Solver
solveChallenge2 = show . length . filter isNiceString2 . lines

challengeSolver :: Challenge
challengeSolver = mkChallenge "challenge-05" solveChallenge1 solveChallenge2
