
module Day9 where

import Data.List (nub, sort)

type Point = (Int,Int) 
type Heights = (Point, Int)
------------------------------------------------------------------------------------------
-- Day 9: Smoke Basin
------------------------------------------------------------------------------------------
getTxtContent :: IO [[Int]] 
getTxtContent = do 
    strings <- lines <$> readFile "day9inp.txt" 
    let xs = stringBreaker strings
    return xs

stringBreaker :: [String] -> [[Int]] 
stringBreaker strings = [map (read.pure :: Char -> Int) str | str <- strings]
------------------------------------------------------------------------------------------
-- Part one
------------------------------------------------------------------------------------------

partOne :: IO Int 
partOne = do 
    mx <- getTxtContent 
    let x = sum.map (+1) $ (snd.unzip.isSmallerThanSurrounding) mx 
    return x

isSmallerThanSurrounding :: [[Int]] -> [Heights] 
isSmallerThanSurrounding mx = 
    concat [[e | e <- line, checkSurrounding e matrix] | line <- matrix]
    where matrix = remodeled (0,0) mx

checkSurrounding :: Heights -> [[Heights]] -> Bool 
checkSurrounding pointHeight matrix = and $ map (\y -> x < y) xs
    where 
        xs = snd.unzip $ borderConditions (fst pointHeight) matrix
        x  = snd pointHeight

borderConditions :: Point -> [[Heights]] -> [Heights]
borderConditions (x,y) mx 
    | x0 && y0 = [getValue (xP,y) mx, getValue (x,yP) mx] --topleft
    | x0 && yL = [getValue (xP,y) mx, getValue (x,yM) mx] --topright
    | xL && y0 = [getValue (xM,y) mx, getValue (x,yP) mx] --botleft
    | xL && yL = [getValue (xM,y) mx, getValue (x,yM) mx] --botright
    | x0 = [getValue (x,yM) mx, getValue (xP,y) mx, getValue (x,yP) mx]
    | y0 = [getValue (xM,y) mx, getValue (x,yP) mx, getValue (xP,y) mx]
    | xL = [getValue (x,yM) mx, getValue (xM,y) mx, getValue (x,yP) mx]
    | yL = [getValue (xM,y) mx, getValue (x,yM) mx, getValue (xP,y) mx]
    | otherwise = [ getValue (xM,y) mx, getValue (xP,y) mx, 
                    getValue (x,yM) mx, getValue (x,yP) mx]
    where 
        (xLen, yLen)     = (length mx - 1 , (length $ head mx) - 1)
        (xM, yM, xP, yP) = (x-1, y-1, x+1, y+1)
        (x0, y0, xL, yL) = (xM < 0, yM < 0, xP > xLen, yP > yLen )


getValue :: Point -> [[a]] -> a
getValue (row,col) matrix = (matrix !! row) !! col

remodeled :: Point -> [[Int]] -> [[Heights]]
remodeled _     []      = []
remodeled (x,y) (m:mx)  = lines (x,y) m : remodeled (x+1, 0) mx
    where 
        lines :: Point -> [Int] -> [Heights] 
        lines _     []      = [] 
        lines (x,y) (l:ls)  = ((x,y), l) : lines (x, y+1) ls


------------------------------------------------------------------------------------------
-- Part two
------------------------------------------------------------------------------------------

partTwo :: IO Int 
partTwo = do 
    mx <- getTxtContent 
    let x  = (product.(take 3).reverse.sort.logSizeBasins) mx
    return x

logSizeBasins :: [[Int]] -> [Int]
logSizeBasins mx = map length [ logBasin b [x] matrix | x@(p,b) <- bottoms ]
    where 
        bottoms = isSmallerThanSurrounding mx 
        matrix  = remodeled (0,0) mx

logBasin :: Int -> [Heights] -> [[Heights]] -> [Heights]
logBasin 9 bs _  = bs
logBasin n bs mx = logBasin (n+1) newBasin mx
    where 
        newBasin = (noNine.qSortBySnd.nubHeights.concat) compIt
        compIt   = [if b == n then basin hh mx else [hh] | hh@(p,b) <- bs]
        noNine   = filter (\x@(p,b) -> b < 9)

basin :: Heights -> [[Heights]] -> [Heights]
basin h@( p, _ ) mx = h : borderConditions p mx

nubHeights :: [Heights] -> [Heights] 
nubHeights []       = [] 
nubHeights (h@(p,he):hs)    | p `elem` tpls = nubHeights hs
                            | otherwise     = h : nubHeights hs 
    where tpls = fst $ unzip hs


qSortBySnd :: Ord b => [(a,b)] -> [(a,b)]
qSortBySnd []       = [] 
qSortBySnd (p:xs)   = qSortBySnd smaller ++ [p] ++ qSortBySnd bigger
    where 
        smaller = [x | x <- xs, snd x < snd p] 
        bigger  = [x | x <- xs, snd x >= snd p] 

------------------------------------------------------------------------------------------
-- TESTS
------------------------------------------------------------------------------------------
tt =   ["2199943210",
        "3987894921",
        "9856789892",
        "8767896789",
        "9899965678"]

bb = remodeled (0,0)  $ stringBreaker tt 

b1 = borderConditions (0,0) bb
b2 = borderConditions (0,9) bb
b3 = borderConditions (4,0) bb
b4 = borderConditions (4,9) bb

b5 = borderConditions (0,2) bb
b6 = borderConditions (1,0) bb
b7 = borderConditions (4,1) bb
b8 = borderConditions (1,9) bb

b9 = borderConditions (1,1) bb

t1 = sum.map (+1) $ (snd.unzip.isSmallerThanSurrounding) $ stringBreaker tt
t2 = (isSmallerThanSurrounding . stringBreaker) tt
t3 = head $ head [basin x bb | x <- t2]
t4 = let x@(p,b) = t3 in logBasin b [x] bb
t5 = logSizeBasins $ stringBreaker tt

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
