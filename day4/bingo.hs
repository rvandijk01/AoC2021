module Bingo where

import Data.List
import Data.List.Split

-- -1 means marked
type Board = [[Int]]

testBoard :: Board
testBoard = [[1,3,5,1,1],[2,1,1,1,1],[6,2,1,4,1],[9,-1,-1,-1,-1],[1,2,2,2,1]]

inputFile :: [Char]
inputFile = "input4.txt"

readLines :: FilePath -> IO [String]
readLines p = filter (not . (== "")) <$> lines <$> readFile p

makeBingoSeq :: [Char] -> [Int]
makeBingoSeq s = read <$> splitOn "," s

makeBoards :: [[Char]] -> [Board]
makeBoards [] = []
makeBoards ss = (makeBoard (take 5 ss)) : (makeBoards $ drop 5 ss) where
    makeBoard ss = map (map read) $ map (filter (not . (== ""))) (map (splitOn " ") ss)

checkBoard :: Board -> Bool
checkBoard xss = check xss || check (transpose xss) || diag xss || diag (reverse xss) where
    check yss = foldl (||) False $ map (foldl (&&) True) $ map (map (== -1)) yss
    diag yss = foldl (\b i -> b && ((yss !! i !! i) == -1)) True [0..(length yss)-1]

fill :: Int -> Board -> Board
fill n b = map (map (\val -> if val == n then -1 else val)) b

-- tick :: Int -> [Board] -> Maybe Board
-- tick n [] = Nothing
-- tick n (b:bs) = if checkBoard $ fill n b then Just $ fill n b else tick n bs

solve :: [Int] -> [Board] -> (Board, Int)
solve xs bs
    | checkBoards bs /= [] = (head (checkBoards bs), head xs)
    | otherwise = solve (tail xs) (map (fill $ head xs) bs)
    where checkBoards = filter (checkBoard)

computeScore :: (Board, Int) -> Int
computeScore (b, x) = (*) 44 $ foldl (+) 0 $ filter (/= -1) $ concat b

-- final :: IO Int
-- final = computeScore <$> answer4_2

answer4_1 :: IO (Board, Int)
answer4_1 = solve <$> (makeBingoSeq <$> head <$> lines) <*> (makeBoards <$> drop 1 <$> lines) where
    lines = readLines inputFile

solve2 :: [Int] -> [Board] -> (Board, Int)
solve2 [] bs = (head bs, -1)
--solve2 xs [b] = (b, head xs)
solve2 xs bs
    | checkBoards bs /= [] = solve2 (tail xs) (map (fill $ head xs) (filter (not . checkBoard) bs))
    | otherwise = solve2 (tail xs) (map (fill $ head xs) bs)
    where checkBoards = filter (not . checkBoard)

lastWinning :: [Int] -> [Board] -> (Board, Int)
lastWinning xs bs
    | (length (checkBoards bs) == 1) && (checkBoard (head bs)) = (head (checkBoards bs), head xs)
    | length (checkBoards bs) == 1 = lastWinning (tail xs) (map (fill $ head xs) (checkBoards bs))
    | length (checkBoards bs) > 1 = lastWinning (tail xs) (map (fill $ head xs) (checkBoards bs))
    | otherwise = error "weird thing happened"
    where checkBoards = filter (not . checkBoard) --checkBoards bs => all boards that are not winning

answer4_2 :: IO (Board, Int)
answer4_2 = lastWinning <$> (makeBingoSeq <$> head <$> lines) <*> (makeBoards <$> drop 1 <$> lines) where
    lines = readLines inputFile

--21 last number called
--[[-1,-1,24,-1,-1],[-1,94,-1,-1,-1],[-1,1,-1,-1,-1],[-1,-1,-1,-1,-1],[85,56,-1,-1,54]]
