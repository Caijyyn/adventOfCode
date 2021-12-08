
module Day8 where

import Data.List (nub, sort, isSubsequenceOf, (\\) )
import Data.List.Split (splitOn)


type EntOut = ([String], [String])
type Segment7 = (Int, String)
------------------------------------------------------------------------------------------
-- Day 8: Seven Segment Search
------------------------------------------------------------------------------------------
getTxtContent :: IO [EntOut]
getTxtContent = do 
    strings <- lines <$> readFile "day8inp.txt"
    let eo = map splitEntryOutput strings
    return eo

splitEntryOutput :: String -> EntOut
splitEntryOutput str = (words $ head spl, words $ last spl) 
    where        spl = splitOn " | " str

------------------------------------------------------------------------------------------
-- Part one
------------------------------------------------------------------------------------------

partOne :: IO Int 
partOne = do 
    entOut <- getTxtContent 
    let output = concat $ snd $ unzip entOut
    let x = occurenceInOutput1478 output
    return x

occurenceInOutput1478 :: [String] -> Int 
occurenceInOutput1478 output = length xs
    where xs = filter (\x -> x `elem` [1,4,7,8]) $ fst $ unzip $ map decodeLines output

decodeLines :: String -> Segment7
decodeLines str 
    | l == 2    = (1,sort str) 
    | l == 4    = (4,sort str) 
    | l == 3    = (7,sort str) 
    | l == 7    = (8,sort str)
    | otherwise = (11, " ")
    where l = length str

------------------------------------------------------------------------------------------
-- Part two
------------------------------------------------------------------------------------------

partTwo :: IO Int 
partTwo = do 
    entOut <- getTxtContent 
    let x = sum $ map evaluateOutput entOut
    return x

evaluateOutput :: EntOut -> Int
evaluateOutput (entry, output) = a*1000 + b*100 + c*10 + d
    where 
        [a,b,c,d] = [convertOutput decoded str | str <- sortedOutp]
        sortedOutp = map sort output
        decoded = decodeEntry entry

        convertOutput :: [Segment7] -> String -> Int
        convertOutput [] _ = error "this should not be able to happen"
        convertOutput ((d,e):codedEntry) output
            | e == output = d 
            | otherwise   = convertOutput codedEntry output

decodeEntry :: [String] -> [Segment7] 
decodeEntry entry = sort decode
    where 
        decode  = decodeDigits initVal entry
        initVal = sort.filter (\(a,b) -> a `elem` [1,4,7,8]) $ map decodeLines entry

decodeDigits :: [(Int, String)] -> [String] -> [Segment7]
decodeDigits tp [] = tp ++ []
decodeDigits tp (s:sx) 
    | zero      = (0,sort s) : decodeDigits tp sx
    | two       = (2,sort s) : decodeDigits tp sx
    | three     = (3,sort s) : decodeDigits tp sx
    | five      = (5,sort s) : decodeDigits tp sx
    | six       = (6,sort s) : decodeDigits tp sx
    | nine      = (9,sort s) : decodeDigits tp sx
    | otherwise = decodeDigits tp sx
    where
        sn    = sort.nub
        len5  = length s == 5
        len6  = length s == 6
        zero  = len6 && ((sn $ one ++ (eight \\ four)) `isSubsequenceOf` (sort s))
        two   = len5 && ((sn $ eight \\ four) `isSubsequenceOf` (sort s))
        three = len5 && ((sn $ one) `isSubsequenceOf` (sort s))
        five  = len5 && ((sn $ (seven \\ one) ++ (four \\ one)) `isSubsequenceOf` (sort s))
        six   = len6 && ((sn $ eight \\ one) `isSubsequenceOf` (sort s))
        nine  = len6 && ((sn $ seven ++ four) `isSubsequenceOf` (sort s))
        (one,four,seven,eight) = (snd $ tp !! 0, snd $ tp !! 1, snd $ tp !! 2, snd $ tp !! 3)


------------------------------------------------------------------------------------------
-- TESTS
------------------------------------------------------------------------------------------
tt  = splitEntryOutput "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
ttt = [
    "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe",
    "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc",
    "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg",
    "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb",
    "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea",
    "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb",
    "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe",
    "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef",
    "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb",
    "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
    ]

t1 = occurenceInOutput1478 $ concat $ snd $ unzip $ map splitEntryOutput ttt
t2 = evaluateOutput tt

n1 = "ab" 
n4 = "abfe" 
n7 = "abd" 
n8 = "abcdefg"

n2 = "acdfg" 
n3 = "abcdf"
n5 = "bcdef" 
n6 = "bcdefg" 
n9 = "abcdef"

s0 = n1 ++ (n8 \\ n4)
s2 = n8 \\ n4
s3 = n1 
s5 = (n7 \\ n1) ++ (n4 \\ n1)
s6 = n8 \\ n1
s9 = n1 ++ s5  

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
