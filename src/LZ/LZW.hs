{- |
  Module      : LZ.LZW
  Description : An implementation of LZW method
  Maintainer  : ???
-}
module LZ.LZW(compress, uncompress) where

import LZ.Dictionaries (empty, ascii, zeroAsChar)
import Data.List (elemIndex)

type Dictionary = [String]

-- | LZW compress method

compress :: String -> [Int]
compress [] = []
compress text = compress' "" ascii text []

compress' :: String -> Dictionary -> String -> [Int] -> [Int]
compress' acc dict [] output
  | acc == [] = output
	| stringInDictionary dict acc = output++[indexOf acc dict]
	| otherwise = output++[indexOf (init acc) dict]++[indexOf [(last acc)] dict]
compress' "" dict (x:xs) output = compress' [x] dict xs output                -- CASE 1 : First reading of the input
compress' acc dict (x:xs) output                                              -- CASE 2 : Reading of the input
	| stringInDictionary dict acc = aux acc dict (x:xs) output	                -- SUB-CASE 2.a : The current string (acc) is in the Dictionary
	| otherwise = compress' ([(last acc)]++[x]) (dict++[acc]) xs (output++[indexOf (init acc) dict])  	-- SUB-CASE 2.b : The current string (acc) is not in the Dictionary


--Aux :: String -> Dictionary -> String -> [Int]
--Pour prévenir si length(acc) > 3 et acc++[x] € dict par exemple dans "belle echellelle"
aux acc dict (x:xs) output
  | stringInDictionary dict (acc++[x]) = compress' (acc++[x]) dict xs output
  | otherwise = compress' (acc++[x]) dict xs output

-- Get the index of an element in a Dictionary
indexOf :: String -> Dictionary -> Int
indexOf element dict =
  case elemIndex element dict of
    Just index -> index
    Nothing     -> -1

-- Verify if a String is in a Dictionary
stringInDictionary :: Dictionary -> String -> Bool
stringInDictionary dict str = str `elem` dict


valueAtIndex :: Int -> Dictionary -> String
valueAtIndex i dict = dict !! i


-- | LZW uncompress method
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: [Int] -> Maybe String
uncompress array = uncompress' [] array ascii []

uncompress' :: String -> [Int] -> Dictionary -> String -> Maybe String
uncompress' w [] dict output = Just output
uncompress' w (x:xs) dict [] = uncompress' (valueAtIndex x dict) xs dict (valueAtIndex x dict )

uncompress' w (x:xs) dict output
  | 0 < x && x < length dict = uncompress' (valueAtIndex x dict) xs (dict++[w++[head (valueAtIndex x dict)]]) (output++(valueAtIndex x dict))
  | x == length dict = uncompress' (w++[head w]) xs (dict++[w++[head (w++[head w])]]) (output++(w++[head w]))
  | otherwise = Nothing