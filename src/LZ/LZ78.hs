{- |
  Module      : LZ.LZ78
  Description : An implementation of LZ78 method
  Maintainer  : ???
-}
module LZ.LZ78(compress, uncompress) where
import Data.List (nub)
import LZ.Dictionaries (empty, zeroAsChar)

-- | LZ78 compress method
compress :: String -> [(Int, Char)]
compress input = compress' (findMatches input "" []) []

compress' :: [String] -> [(Int, Char)] -> [(Int, Char)]
compress' [] dict = dict
compress' (x:xs) dict = compress' xs (dict ++ [((findIndexInDict x dict 0 0),last x)])
-- pour le dictionnaire, on prend le dernier char du string et on cherche dans le dict tout ce qui se trouve avant
findMatches :: String -> String -> [String] -> [String]
findMatches [] temp output = (output ++ [temp]) --Pour ne pas perdre d'info pour la dernière chaine de caractère, même si elle est déjà présente
findMatches (x:xs) "" output = findMatches xs [x] output
findMatches (x:xs) temp output
  | temp `elem` output = findMatches xs (temp ++ [x]) output
  | otherwise = findMatches (x:xs) [] (output ++ [temp]) 

findIndexInDict :: String -> [(Int, Char)] -> Int -> Int -> Int
findIndexInDict s [] count formerIndex = formerIndex
findIndexInDict (s:ss) ((a,b):xs) count formerIndex
  | length (s:ss) == 1 = formerIndex
  | s == b && formerIndex == a = findIndexInDict ss xs (count + 1) (count + 1)
  | otherwise = findIndexInDict (s:ss) xs (count + 1) formerIndex
  
--Pour décompression on construit la liste des chaines et pour (3,'e') on fait L[3-1]++'e'

-- | LZ78 uncompress method
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: [(Int, Char)] -> Maybe String
uncompress compressed = Just (concat (extractList compressed []))

extractList :: [(Int, Char)] -> [String] -> [String]
extractList [] l = l
extractList ((a,b):xs) l 
  | a == 0 = extractList xs (l ++ [[b]])
  | otherwise = extractList xs (l ++ [(l !! (a-1)) ++ [b]])