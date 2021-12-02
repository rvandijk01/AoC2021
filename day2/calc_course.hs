module CourseCalculation where

import Data.List

inputFile :: [Char]
inputFile = "input2.txt"

readLines :: FilePath -> IO [String]
readLines p = lines <$> readFile p

data Com a = F a | D a | U a deriving Show

readCommands :: FilePath -> IO [Com Int]
readCommands p = (map parseCommand) <$> readLines p where
    parseCommand cs = if isPrefixOf "forward" cs then F r else
                      if isPrefixOf "down" cs then D r else
                      if isPrefixOf "up" cs then U r else F 0 where
                          r = (read [last cs])

foldCom1 :: [Com Int] -> (Int, Int)
foldCom1 xs = foldl f (0,0) xs where
    f (x,y) (F a) = (x+a,y)
    f (x,y) (D a) = (x,y+a)
    f (x,y) (U a) = (x,y-a)

answer2_1 :: IO Int
answer2_1 = (uncurry (*)) <$> foldCom1 <$> readCommands inputFile

foldCom2 :: [Com Int] -> (Int, Int, Int)
foldCom2 xs = foldl f (0,0,0) xs where
    f (a,x,y) (F v) = (a,x+v,y+(a*v))
    f (a,x,y) (D v) = (a+v,x,y)
    f (a,x,y) (U v) = (a-v,x,y)

answer2_2 :: IO Int
answer2_2 = (\(a,x,y) -> x*y) <$> foldCom2 <$> readCommands inputFile