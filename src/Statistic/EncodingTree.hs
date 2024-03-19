{- |
  Module : Statistic.EncodingTree
  Description : A module representing a binary tree for binary encoding
  Maintainer : ???
-}
module Statistic.EncodingTree(EncodingTree(..), isLeaf, count, has, encode, decodeOnce, decode, meanLength, compress, uncompress) where

import Statistic.Bit

data EncodingTree a = EncodingNode Int (EncodingTree a) (EncodingTree a)
                    | EncodingLeaf Int a
  deriving (Eq, Show)

-- | Is the encoding a mere leaf ?
isLeaf :: EncodingTree a -> Bool
isLeaf (EncodingLeaf _ _) = True
isLeaf  _                 = False

-- | The length of the underlying source
count :: EncodingTree a -> Int
count (EncodingLeaf cnt _  ) = cnt
count (EncodingNode cnt _ _) = cnt

--TODO
-- | Search for symbol in encoding tree
has :: Eq a => EncodingTree a -> a -> Bool
has (EncodingLeaf _ x) symbol 
  |x == symbol = True
  |x /= symbol = False
-- has (EncodingLeaf _ x) symbol = x == symbol marche aussi et est plut court
has (EncodingNode _ left right) symbol = has left symbol || has right symbol 

-- | Computes the binary code of symbol using encoding tree
-- If computation is not possible, returns `Nothing`.
encode :: Eq a => EncodingTree a -> a -> Maybe [Bit]
encode (EncodingLeaf _ x) symbol
  | x == symbol = Just [] -- Le symbole est dans l'arbre, on ajouter rien à la liste
  | otherwise = Nothing -- Le symbole n'est pas danc l'arbre
encode (EncodingNode _ left right) symbol
  | has left symbol = fmap (Zero:) (encode left symbol) -- On ajouter Zero à la liste, on cherche à gauche
  | has right symbol = fmap (One:) (encode right symbol) -- On ajoute One à la list, on cherche à droite
  | otherwise = Nothing -- Le symbole n'est pas danc l'arbre


-- | Computes the first symbol from list of bits using encoding tree and also returns the list of bits still to process
-- If computation is not possible, returns `Nothing`.
decodeOnce :: EncodingTree a -> [Bit] -> Maybe (a, [Bit])
decodeOnce (EncodingLeaf _ symbol) bits = Just (symbol, bits)-- Fin de l'arbre
decodeOnce (EncodingNode _ left right) (bit:bits)-- On sépare l'array de bits avec [bit,bits]
  | bit == Zero = decodeOnce left bits
  | bit == One = decodeOnce right bits
decodeOnce _ _ = Nothing

-- | Computes list of symbols from list of bits using encoding tree
decode :: EncodingTree a -> [Bit] -> Maybe [a]
decode _ [] = Just []  -- Cas de base
decode tree bits
    | Just (symbol, remainingBits) <- decodeOnce tree bits = fmap (symbol :) (decode tree remainingBits)
    | otherwise = Nothing

-- | Mean length of the binary encoding
meanLength :: EncodingTree a -> Double
meanLength tree = foldl (\acc (len, depth) -> acc + fromIntegral (len * depth) / fromIntegral numberNodes) 0 leafLengthsAndDepths
  where
    leafLengthsAndDepths = numbersLeafs tree 0
    numberNodes = count tree

    numbersLeafs :: EncodingTree a -> Int -> [(Int, Int)]
    numbersLeafs (EncodingLeaf cnt _) depth = [(cnt, depth)]
    numbersLeafs (EncodingNode _ left right) depth = numbersLeafs left (depth + 1) ++ numbersLeafs right (depth + 1)

-- | Compress method using a function generating encoding tree and also returns generated encoding tree
-- Correction dans compress pour utiliser concatMap
compress :: Eq a => ([a] -> Maybe (EncodingTree a)) -> [a] -> (Maybe (EncodingTree a), [Bit])
compress encodingTree text = (encodingTreeResult, encodedText)
  where
    encodingTreeResult = encodingTree text

    encodedText = case encodingTreeResult of
        Just tree -> concatMap (fromMaybe [] . encode tree) text
        Nothing -> []

    fromMaybe defVal maybeVal = case maybeVal of
                    Just val -> val
                    Nothing -> defVal

-- | Uncompress method using previously generated encoding tree
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: (Maybe (EncodingTree a), [Bit]) -> Maybe [a]
uncompress (Just tree, bits) = decode tree bits
uncompress _ = Nothing
