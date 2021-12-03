module PowerOutput where

import Data.List

inputFile :: [Char]
inputFile = "input3.txt"

readLines :: FilePath -> IO [String]
readLines p = lines <$> readFile p

readCodes :: FilePath -> IO [[Bool]]
readCodes p = (map $ map (\c -> c == '1')) <$> readLines p

mostCommon :: [[Bool]] -> [Bool]
mostCommon m = map (>0) $ map (\xs -> foldl (\acc b -> if b then acc+1 else acc-1) 0 xs) m

--requires reversed list as input
getNum :: Int -> [Bool] -> Int
getNum _ [] = 0
getNum n (b:bs) = (if b then 2^n else 0) + (getNum (n+1) bs)

answer3_1 :: IO Int
answer3_1 = (*) <$> (getNum 0 <$> rev) <*> (getNum 0 <$> (map not <$> rev)) where
    rev = reverse <$> mostCommon <$> (transpose <$> readCodes inputFile)

mostCommon1 :: [Bool] -> Bool
mostCommon1 xs = (foldl (\acc b -> if b then acc+1 else acc-1) 0 xs) >= 0

filterOnBit :: Int -> Bool -> [[Bool]] -> [[Bool]]
filterOnBit n b [] = []
filterOnBit n b (x:xs) = if b == (x !! n) then x : rest else rest where
    rest = filterOnBit n b xs

oxyGenR :: Int -> [[Bool]] -> [[Bool]]
oxyGenR pos [] = []
oxyGenR pos [x] = [x]
oxyGenR pos (x:xs) = oxyGenR (pos+1) filtered where
    filtered = filterOnBit pos (mostCommon1 $ (transpose (x:xs)) !! pos) (x:xs)

scrubCO2 :: Int -> [[Bool]] -> [[Bool]]
scrubCO2 pos [] = []
scrubCO2 pos [x] = [x]
scrubCO2 pos (x:xs) = scrubCO2 (pos+1) filtered where
    filtered = filterOnBit pos (not $ mostCommon1 $ (transpose (x:xs)) !! pos) (x:xs)

answer3_2 :: IO Int
answer3_2 = (*) <$> (getNum 0 <$> oxy) <*> (getNum 0 <$> co2) where
    oxy = reverse <$> head <$> oxyGenR 0 <$> (readCodes inputFile)
    co2 = reverse <$> head <$> scrubCO2 0 <$> (readCodes inputFile)