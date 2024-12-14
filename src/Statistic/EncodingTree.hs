{- |
  Module : Statistic.EncodingTree
  Description : A module representing a binary tree for binary encoding
  Maintainer : ???
-}
module Statistic.EncodingTree(EncodingTree(..), isLeaf, count, has, encode, decodeOnce, decode, meanLength, compress, uncompress) where

import Statistic.Bit
import Data.Maybe()
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

-- | Search for symbol in encoding tree
has :: Eq a => EncodingTree a -> a -> Bool
has (EncodingLeaf _ x) symbol = x == symbol
has (EncodingNode _ left right) symbol = has left symbol || has right symbol

-- | Computes the binary code of symbol using encoding tree
-- If computation is not possible, returns `Nothing`.
encode :: Eq a => EncodingTree a -> a -> Maybe [Bit]
encode tree bits
  | isMeanLengthZero tree = encodeForLeaf tree bits
  | otherwise = encodeMain tree bits
-- Fonction utilisée pour tout les arbres, sauf ceux avec une seule feuille
encodeMain :: Eq a => EncodingTree a -> a -> Maybe [Bit]
encodeMain (EncodingLeaf _ x) symbol
  | x == symbol = Just [] -- Le symbole est dans l'arbre, on ajouter rien à la liste
  | otherwise = Nothing -- Le symbole n'est pas danc l'arbre
encodeMain (EncodingNode _ left right) symbol
  | has left symbol = fmap (Zero:) (encodeMain left symbol) -- On ajouter Zero à la liste, on cherche à gauche
  | has right symbol = fmap (One:) (encodeMain right symbol) -- On ajoute One à la list, on cherche à droite
  | otherwise = Nothing -- Le symbole n'est pas danc l'arbre
-- Fonction utilisée uniquement pour les arbres avec qu'une seule feuille
encodeForLeaf :: Eq a => EncodingTree a -> a -> Maybe [Bit]
encodeForLeaf (EncodingLeaf _ x) symbol
  | x == symbol = Just [Zero] -- Le symbole est dans l'arbre, on ajouter rien à la liste
  | otherwise = Nothing -- Le symbole n'est pas danc l'arbre
encodeForLeaf (EncodingNode _ _ _) _ = Nothing

-- | Computes the first symbol from list of bits using encoding tree and also returns the list of bits still to process
-- If computation is not possible, returns `Nothing`.
decodeOnce :: EncodingTree a -> [Bit] -> Maybe (a, [Bit])
decodeOnce tree bits
  | isMeanLengthZero tree = decodeOnceForLeaf tree bits
  | otherwise = decodeOnceMain tree bits
-- Fonction utilisée pour tout les arbres, sauf ceux avec une seule feuille
decodeOnceMain :: EncodingTree a -> [Bit] -> Maybe (a, [Bit])
decodeOnceMain (EncodingLeaf _ symbol) bits = Just (symbol, bits)-- Fin de l'arbre
decodeOnceMain (EncodingNode _ left right) (bit:bits)-- On sépare l'array de bits avec [bit,bits]
  | bit == Zero = decodeOnceMain left bits
  | bit == One = decodeOnceMain right bits
decodeOnceMain _ _ = Nothing
decodeOnceForLeaf :: EncodingTree a -> [Bit] -> Maybe (a, [Bit])
decodeOnceForLeaf (EncodingLeaf _ symbol) (_:t) = Just (symbol, t) -- On a directement une feuille normalement
decodeOnceForLeaf (EncodingLeaf _ symbol) []     = Just (symbol, []) -- When there is a leaf and no more bits
decodeOnceForLeaf (EncodingNode _ _ _) _        = Nothing

-- Fonction pour tester si la taille de l'arbre d'encodage est de 0
isMeanLengthZero :: EncodingTree a -> Bool
isMeanLengthZero tree = meanLength tree == 0

-- | Computes list of symbols from list of bits using encoding tree
decode :: EncodingTree a -> [Bit] -> Maybe [a]
decode tree bits
  | isMeanLengthZero tree = decodeForLeaf tree bits
  | otherwise = decodeMain tree bits
-- Fonction utilisée pour tout les arbres, sauf ceux avec une seule feuille
decodeMain :: EncodingTree a -> [Bit] -> Maybe [a]
decodeMain _ [] = Just []  -- Cas de base
decodeMain tree bits
    | Just (symbol, remainingBits) <- decodeOnceMain tree bits = fmap (symbol :) (decodeMain tree remainingBits)
    | otherwise = Nothing
-- Fonction utilisée uniquement pour les arbres avec qu'une seule feuille
decodeForLeaf :: EncodingTree a -> [Bit] -> Maybe [a]
decodeForLeaf _ [] = Just []  -- Cas de base
decodeForLeaf tree bits
    | Just (symbol, remainingBits) <- decodeOnceForLeaf tree bits = fmap (symbol :) (decodeForLeaf tree remainingBits)
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
