module IncreasingDepths where

inputFile :: [Char]
inputFile = "input1.txt"

readLines :: FilePath -> IO [String]
readLines p = lines <$> readFile p

readNumbers :: FilePath -> IO [Int]
readNumbers p = (map read) <$> readLines p

countIncreasing :: [Int] -> Int
countIncreasing [] = 0
countIncreasing [x] = 0
countIncreasing (x:xs) = if x < (head xs) then 1 + rest else 0 + rest
    where rest = countIncreasing xs

answer1 :: IO Int
answer1 = countIncreasing <$> readNumbers inputFile

windows :: Int -> [Int] -> [Int]
windows s xs = if (length win) < s then [] else (sum win) : (windows s (tail xs))
    where win = (take s xs)

answer2 :: IO Int
answer2 = countIncreasing <$> (windows 3) <$> readNumbers inputFile