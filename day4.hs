
module Day4 where

import Data.List (transpose, elemIndices)
import Data.List.Split (splitOn, chunksOf)

type Sheet = [[Int]] 
------------------------------------------------------------------------------------------
-- Day 4: Giant Squid -- BINGO! --
------------------------------------------------------------------------------------------
getTxtContent :: IO ( [Int], [Sheet] ) 
getTxtContent = do
    strs <- map lines . words <$> readFile "day4inp.txt"
    let xs = remodelData strs
    return xs

remodelData :: [[String]] -> ( [Int], [Sheet] )
remodelData inp = (noDrawList inp, createBingoSheets inp)

createBingoSheets :: [[String]] -> [Sheet]
createBingoSheets inp = chunksOf 5 $ chunksOf 5 sht
    where         sht = map (read :: String -> Int) $ map head $ tail inp

noDrawList :: [[String]] -> [Int]
noDrawList inp = map (read :: String -> Int) ind
    where 
        ind = splitOn "," str
        str = head $ head inp

------------------------------------------------------------------------------------------
-- Part one
------------------------------------------------------------------------------------------
partOne :: IO Int 
partOne = do 
    (a,b) <- getTxtContent 
    let x = bingoScore $ drawNumber (drop 5 a, b) (take 5 a) 
    return x

bingoScore :: (Int, [Int], Sheet) -> Int 
bingoScore (lastNo, drawn, sheet) = lastNo * (sum unmarked)
    where 
        unmarked  = takeUnmarked drawn flattened []
        flattened = concat sheet

takeUnmarked :: [Int] -> [Int] -> [Int] -> [Int]
takeUnmarked  _  [] unmarked = unmarked
takeUnmarked drawn (s:heet) unmarked 
    | notElem s drawn = takeUnmarked drawn heet (s:unmarked)
    | otherwise       = takeUnmarked drawn heet unmarked

drawNumber :: ( [Int], [Sheet] ) -> [Int] -> (Int, [Int], Sheet)
drawNumber ((a:as),b) xs 
    | horisontal = (head xs, xs, findSheet b find1)
    | vertical   = (head xs, xs, findSheet b find2)
    | otherwise  = drawNumber (as,b) (a:xs)
    where 
        (horisontal , find1) = checkHorisontal b xs
        (vertical   , find2) = checkVertical   b xs

findSheet :: [Sheet] -> [Bool] -> Sheet 
findSheet sheets bingo = let i = head $ elemIndices True bingo in sheets !! i

checkHorisontal, checkVertical :: [Sheet] -> [Int] -> (Bool, [Bool]) 
checkHorisontal sheets drawn = (checkSheets, allSheets)
    where
        checkSheets   = elem (True) allSheets
        allSheets     = [elem (True) sheet | sheet <- checkRows]
        checkRows     = [[if row == validRow then True else False | row <- sheet] | sheet <- sheetsVSdrawn]
        sheetsVSdrawn = [[[ elem num drawn | num <- row] | row <- sheet] | sheet <- sheets]
        validRow      = [True, True, True, True, True]

checkVertical sheets drawn = checkHorisontal vertical drawn
    where       vertical   = map transpose sheets

------------------------------------------------------------------------------------------
-- Part two
------------------------------------------------------------------------------------------

partTwo :: IO Int 
partTwo = do 
    (a,b) <- getTxtContent 
    let x = bingoScore $ lastSheet (drop 1 a, b) (take 1 a) 
    return x

lastSheet :: ( [Int], [Sheet] ) -> [Int] -> (Int, [Int], Sheet)
lastSheet ( _ , [x]) xs = (head xs, xs,  x)
lastSheet ((a:as), b) xs 
    | horisontal = lastSheet (as, deleteAtIndex b find1) (a:xs)
    | vertical   = lastSheet (as, deleteAtIndex b find2) (a:xs)
    | otherwise  = lastSheet (as, b) (a:xs)
    where 
        (horisontal , find1) = checkHorisontal b xs
        (vertical   , find2) = checkVertical   b xs

deleteAtIndex :: [Sheet] -> [Bool] -> [Sheet] 
deleteAtIndex sheets bingo = newSheets
    where 
        (idx, newSheets) = unzip [(a,b) | (a,b) <- zipped, a `notElem` is]
        zipped           = indexes `zip` sheets
        indexes          = take (length bingo) [0..]
        is               = elemIndices True bingo

------------------------------------------------------------------------------------------
-- TESTS
------------------------------------------------------------------------------------------
tt1 :: [Int]
tt1 = [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1]
tt2 :: [Sheet]
tt2 = [[[22, 13, 17, 11,  0],
        [ 8,  2, 23,  4, 24],
        [21,  9, 14, 16,  7],
        [ 6, 10,  3, 18,  5],
        [ 1, 12, 20, 15, 19]],

        [[3, 15,  0,  2, 22],
        [ 9, 18, 13, 17,  5],
        [19,  8,  7, 25, 23],
        [20, 11, 10, 24,  4],
        [14, 21, 16, 12,  6]],

        [[14, 21, 17, 24, 4],
        [10, 16, 15,  9, 19],
        [18,  8, 23, 26, 20],
        [22, 11, 13,  6,  5],
        [ 2,  0, 12,  3,  7]]
        ]

test1 = bingoScore $ drawNumber (drop 5 tt1, tt2) (take 5 tt1)
test2 = bingoScore $ lastSheet (drop 5 tt1, tt2) (take 5 tt1)

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
