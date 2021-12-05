
module Day3 where

import Data.List (transpose)

------------------------------------------------------------------------------------------
-- Day 3: Binary Diagnostic
------------------------------------------------------------------------------------------
getTxtContent :: IO [[Int]]
getTxtContent = do
    xs <- map lines . words <$> readFile "day3inp.txt"
    let str = map head xs
    let bin = [map (read . pure :: Char -> Int) bs | bs <- str]
    return bin

------------------------------------------------------------------------------------------
-- Part one
------------------------------------------------------------------------------------------

partOne :: IO Int 
partOne = do 
    xs <- getTxtContent 
    let x = calculateIt xs 
    return x

calculateIt, gamma, epsilon :: [[Int]] -> Int 
calculateIt bin = gamma bin * epsilon bin
gamma       bin = binaryToNum $ newBinary bin
epsilon     bin = binaryToNum $ map oneComp $ newBinary bin

newBinary :: [[Int]] -> [Int]
newBinary bin = newBin
    where 
        newBin  = [if a > b then 1 else 0 | (a,b) <- counted]
        counted = map counter trpsed
        trpsed  = transpose bin

binaryToNum :: [Int] -> Int 
binaryToNum bs = sum $ convert bs 
    where 
        convert :: [Int] -> [Int] 
        convert []     = [] 
        convert (x:xs) = x * 2 ^ length xs : convert xs

oneComp :: Int -> Int 
oneComp 0 = 1
oneComp 1 = 0
oneComp _ = error "oneComp: Not binary"

counter :: [Int] -> (Int, Int)
counter bin = ( mostCommon bin 1 , mostCommon bin 0 )
    where 
        mostCommon :: [Int] -> Int -> Int  
        mostCommon bs b = length $ filter (\x -> x == b) bs

------------------------------------------------------------------------------------------
-- Part two
------------------------------------------------------------------------------------------

partTwo :: IO Int 
partTwo = do 
    xs <- getTxtContent 
    let x = lifeSupportRating xs 
    return x

lifeSupportRating, co2ScrubRating, oxygenGenRating :: [[Int]] -> Int 
lifeSupportRating bins = co2ScrubRating bins * oxygenGenRating bins 
co2ScrubRating    bins = binaryToNum $ filterBinaries "lsc" 0 bins 
oxygenGenRating   bins = binaryToNum $ filterBinaries "msc" 0 bins

filterBinaries :: String -> Int -> [[Int]] -> [Int]
filterBinaries str _ [x] = x 
filterBinaries str n bin = filterBinaries str (n+1) newBins 
    where 
        newBins = filter (\x -> (x !! n) == bit) bin
        bit     = bitChoice str count
        count   = check !! n
        check   = map counter $ transpose bin

bitChoice :: String -> (Int,Int) -> Int
bitChoice st (x,y)  
    | msc && xL = 1
    | lsc && yL = 1
    | msc && yL = 0
    | lsc && xL = 0
    | otherwise = error "String incorrect, type 'msc' or 'lsc'."
    where 
        (msc, lsc) = ( st == "msc" , st == "lsc" )
        (xL, yL)   = (  x >= y     ,  x <  y )

------------------------------------------------------------------------------------------
-- TESTS
------------------------------------------------------------------------------------------

tt = [  "00100","11110","10110","10111","10101","01111",
        "00111","11100","10000","11001","00010","01010" ]
tb = [map (read . pure :: Char -> Int) bs | bs <- tt]
t1 = calculateIt tb
t2 = lifeSupportRating tb 

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
