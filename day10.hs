
module Day10 where

import Data.List ( sort )

------------------------------------------------------------------------------------------
-- Day 10: Syntax Scoring
------------------------------------------------------------------------------------------
getTxtContent :: IO [String] 
getTxtContent = do 
    strings <- lines <$> readFile "day10inp.txt" 
    return strings


------------------------------------------------------------------------------------------
-- Part one
------------------------------------------------------------------------------------------

partOne :: IO Int 
partOne = do 
    strings <- getTxtContent 
    let x = (sum.givePoints.findIllegals.checkLines) strings 
    return x

givePoints invalids = map scoring invalids
findIllegals strings = filter (\x -> x `notElem` "({[<") (concat strings)
checkLines strings = map (\x -> stackBrackets x []) strings

stackBrackets :: String -> String -> String
stackBrackets [] left = left 
stackBrackets (r:raw) [] 
    | brackets r          = stackBrackets raw [r]
    | otherwise           = [r]
stackBrackets (r:raw) (l:eft)
    | brackets  r         = stackBrackets raw ( r : l:eft)
    | condition l r       = stackBrackets raw eft
    | not $ condition l r = [r]
    -- | otherwise           = [r]

brackets :: Char -> Bool
brackets s = s == '(' || s == '{' || s == '[' || s == '<'

condition :: Char -> Char -> Bool 
condition '(' ')' = True 
condition '{' '}' = True 
condition '[' ']' = True 
condition '<' '>' = True 
condition  _   _  = False

scoring :: Char -> Int 
scoring ')' = 3
scoring ']' = 57
scoring '}' = 1197
scoring '>' = 25137

------------------------------------------------------------------------------------------
-- Part two
------------------------------------------------------------------------------------------
partTwo :: IO Int 
partTwo = do 
    strings <- getTxtContent 
    let xs = fixItThenSortIt . map (reverse.snd) $ (comprehendBrackets.checkLines) strings 
    let x  = xs !! (length xs `div` 2)
    return x

fixItThenSortIt :: [String] -> [Int]
fixItThenSortIt strings = sort $ map (\x -> scoreString x 0) scoreIt
    where scoreIt = map (\x -> map sndScore x) strings

scoreString :: [Int] -> Int -> Int
scoreString [x]    z = (z * 5 + x)
scoreString (x:xs) z = scoreString xs (z * 5 + x)

sndScore :: Char -> Int 
sndScore ')' = 1
sndScore ']' = 2
sndScore '}' = 3
sndScore '>' = 4

comprehendBrackets :: [String] -> [(String, String)]
comprehendBrackets strings = [autoCompleteBrackets x (x,[]) | x <- strings, length x > 1 ]

autoCompleteBrackets :: String -> (String, String) -> (String, String)
autoCompleteBrackets [] (origin , mirror) = (origin , mirror)
autoCompleteBrackets (s:str) (origin , mirror) 
    | s `elem` "({[<" = autoCompleteBrackets str (origin, mirroring s : mirror)
    | otherwise       = error "Make sure to filter out corrupted ones before using this!"
    where 
        mirroring :: Char -> Char 
        mirroring '('  = ')' 
        mirroring '{'  = '}' 
        mirroring '['  = ']' 
        mirroring '<'  = '>' 

------------------------------------------------------------------------------------------
-- TESTS
------------------------------------------------------------------------------------------
tt = [
    "[({(<(())[]>[[{[]{<()<>>"  ,
    "[(()[<>])]({[<{<<[]>>("    ,
    "{([(<{}[<>[]}>{[]{[(<()>"  ,
    "(((({<>}<{<{<>}{[]{[]{}"   ,
    "[[<[([]))<([[{}[[()]]]"    ,
    "[{[{({}]{}}([{[{{{}}([]"   ,
    "{<[[]]>}<{[{[{[]{()[[[]"   ,
    "[<(<(<(<{}))><([]([]()"    ,
    "<{([([[(<>()){}]>(<<{{"    ,
    "<{([{{}}[<[[[<>{}]]]>[]]"  ]

corrupted = [   
    "{([(<{}[<>[]}>{[]{[(<()>"  , 
    "[[<[([]))<([[{}[[()]]]"    ,
    "[{[{({}]{}}([{[{{{}}([]"   ,
    "[<(<(<(<{}))><([]([]()"    ,
    "<{([([[(<>()){}]>(<<{{"    ]

c1 = ( length $ corrupted !! 2, length $ corrupted !! 3 )

t1 = stackBrackets ( corrupted !! 0 ) []
t2 = stackBrackets ( tt !! 0 ) []
t3 = sum.givePoints $ findIllegals $ checkLines tt
t4 = fixItThenSortIt . map (reverse.snd) $ (comprehendBrackets.checkLines) tt
t5 = t4 !! (length t4 `div` 2)
------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
