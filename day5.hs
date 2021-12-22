
module Day5 where

import Data.List.Split (splitOn, chunksOf)
import Data.List (intersect, sort, nub, group)

type Vector = (Int, Int)
type Force  = (Vector, Vector)
type Matrix = [[Int]]

------------------------------------------------------------------------------------------
-- Day 5: Hydrothermal Venture
------------------------------------------------------------------------------------------
getTxtContent :: IO [Force] 
getTxtContent = do
    strs <- map lines . words <$> readFile "day5inp.txt"
    let str = sort $ stringBreaker strs
    return str

stringBreaker :: [[String]] -> [Force]
stringBreaker strings = [ (a,b) | [a,b] <- forces]
    where 
        forces = chunksOf 2 $ map (\[x,y] -> (x,y)) xss
        xss = [map (read :: String -> Int) s | s <- st]
        st  = map (\x -> splitOn "," x) str
        str = filter (\x -> x /= "->") $ map head strings

------------------------------------------------------------------------------------------
-- Part one  -- 
------------------------------------------------------------------------------------------
partOne :: IO Int
partOne = do 
    tpls <- getTxtContent 
    let forces = filterOutVerticalAndHorisontal tpls
    let x = length.filter (\x -> length x > 1) $ (group.sort.concat.drawStraightLines) forces
    return x

filterOutVerticalAndHorisontal :: [Force] -> [Force] 
filterOutVerticalAndHorisontal forces = 
    (sort.filter (\((x1,y1),(x2,y2)) -> (x1 == x2) || (y1 == y2))) forces

drawStraightLines :: [Force] -> [[Vector]]
drawStraightLines [] = []
drawStraightLines (((x1,y1),(x2,y2)):fs) 
    | x1 == x2  = [(x1,y) | y <- (list y1 y2)] : drawStraightLines fs
    | y1 == y2  = [(x,y1) | x <- (list x1 x2)] : drawStraightLines fs
    | otherwise = drawStraightLines fs

list :: Int -> Int -> [Int]
list i1 i2  | i1 <  i2 = [i1..i2]
            | i1 >= i2 = [i2..i1]

------------------------------------------------------------------------------------------
-- Part two
------------------------------------------------------------------------------------------
partTwo :: IO Int 
partTwo = do 
    forces <- getTxtContent
    let vs1 = (drawStraightLines . filterOutVerticalAndHorisontal) forces 
    let vs2 = (drawDiagonal . filterDiagonals) forces 
    let x = length.filter (\x -> length x > 1) $ (group.sort.concat) $ vs1 ++ vs2
    return x

filterDiagonals :: [Force] -> [Force]
filterDiagonals forces = 
    (sort.filter (\((x1,y1),(x2,y2)) -> (x1 /= x2) && (y1 /= y2))) forces

drawDiagonal :: [Force] -> [[Vector]]
drawDiagonal []                     = []
drawDiagonal (f:fs) = direction f : drawDiagonal fs

direction :: Force -> [Vector] 
direction ((x1,y1),(x2,y2)) 
    | x1 > x2 && y1 < y2 = [x1, x1-1 .. x2] `zip` [y1 .. y2]
    | x1 < x2 && y1 > y2 = [x1 .. x2] `zip` [y1, y1-1 .. y2] 
    | otherwise          = list x1 x2 `zip` list y1 y2


------------------------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------------------------

buildMatrix :: Int -> Matrix
buildMatrix n = replicate n $ replicate n 0

plotMatrix :: Matrix -> IO ()
plotMatrix matrix = putStr $ concat $ map (\x -> x ++ "\n") str
    where 
        str = [[if s == '0' then '.' else s | s <- st, unClutter s] | st <- strings]
        strings = map show matrix

        unClutter :: Char -> Bool 
        unClutter c = (c /= ',') && (c /= '[') && (c /= ']')

allPoints :: Int -> [Vector] 
allPoints n = [(x,y) | x <- list, y <- list]
    where list = [0 .. abs n]


------------------------------------------------------------------------------------------
-- TESTS
------------------------------------------------------------------------------------------
tt = [  ["0,9"], ["->"], ["5,9"],
        ["8,0"], ["->"], ["0,8"],
        ["9,4"], ["->"], ["3,4"],
        ["2,2"], ["->"], ["2,1"],
        ["7,0"], ["->"], ["7,4"],
        ["6,4"], ["->"], ["2,0"],
        ["0,9"], ["->"], ["2,9"],
        ["3,4"], ["->"], ["1,4"],
        ["0,0"], ["->"], ["8,8"],
        ["5,5"], ["->"], ["8,2"]]

ttt = allPoints 9

t1 = length.filter (\x -> length x > 1) $ (group.sort.concat.drawStraightLines.filterOutVerticalAndHorisontal.stringBreaker) tt


------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
