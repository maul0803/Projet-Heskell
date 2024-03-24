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

-- Example : "belle echelle"
-- Output = [98, 101, 108, 108, 101, 32, 101, 99, 104, 257, 259]

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
--To prevent when length(acc) > 3 and acc++[x] € dict par exemple dans "belle echellelle"
aux acc dict (x:xs) output
  | stringInDictionary dict (acc++[x]) = compress' (acc++[x]) dict xs output
  | otherwise = compress' (acc++[x]) dict xs output

--stringInDictionary dict acc = function (acc++[x]) dict xs (output++[indexOf acc dict])





-- Get the index of an element in a Dictionary
indexOf :: String -> Dictionary -> Int
indexOf element dict =
  case elemIndex element dict of
    Just index -> index
    Nothing     -> -1


-- Get the length of a Dictionary
sizeOf :: Dictionary -> Int
sizeOf dict = length dict

-- Get the value in a Dictionary at an index
valueAtIndex' :: Dictionary -> Int -> Maybe String
valueAtIndex' dict index
  | index < 0 || index >= length dict = Nothing
  | otherwise = Just(dict !! index)

-- Verify if a String is in a Dictionary
stringInDictionary :: Dictionary -> String -> Bool
stringInDictionary dict str = str `elem` dict





valueAtIndex :: Int -> Dictionary -> String
valueAtIndex i dict = dict !! i





-- | LZW uncompress method
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: [Int] -> Maybe String
uncompress array = uncompress' "" ascii array ""


uncompress' :: String -> Dictionary -> [Int] -> String -> Maybe String

uncompress' acc dict [] output = Just output
uncompress' "" dict (x:xs) output = uncompress' (valueAtIndex x dict) dict xs output
uncompress' acc dict (x:xs) output
  | stringInDictionary dict acc = aux' acc dict (x:xs) output
	| otherwise = uncompress' (last ([acc]++[(valueAtIndex x dict)])) (dict++[acc]) xs (output++(head [acc]))


aux' acc dict (x:xs) output
  | stringInDictionary dict (acc++(valueAtIndex x dict)) = uncompress' (acc++(valueAtIndex x dict)) dict xs output
  | otherwise = uncompress' (acc++(valueAtIndex x dict)) dict xs output