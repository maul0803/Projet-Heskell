{- |
  Module : Statistic.ShannonFano
  Description : A module containing specifics for the Shannon-Fano compression method
  Maintainer : ???
-}
module Statistic.ShannonFanoV1(tree) where
import Statistic.EncodingTree ( EncodingTree(..) )

{-
1. Trier la distribution des symboles rangée par ordre décroissant du
nombre d'apparitions
2. Couper en deux sous-distributions les plus équilibrées possibles
3. Recommencer l'étape 2 sur chaque sous-distribution jusqu'à obtenir
des distributions à un symbole
4. Construire l'arbre à partir des coupes
(Les performances ne sont pas meilleures que celles de Huffman.)
-}

-- Retourne le nombre de fois qu'un symbole apparait dans une liste
elementCount :: Eq a => [a] -> a -> Int
elementCount [] _ = 0
elementCount (h:t) symbol
  | h == symbol = 1 + elementCount t symbol
  | otherwise = 0 + elementCount t symbol
-- Retourne une liste des fréquences de chaque lettre
frequencies :: (Eq a) => [a] -> [(a, Int)]
frequencies [] = []
frequencies (h:t) = (h, elementCount (h:t) h) : frequencies (filter (/= h) t)

-- Retourne la somme de toutes les fréquences
sumOfFrequencies :: [(a, Int)] -> Int
sumOfFrequencies [] = 0
sumOfFrequencies (h:t) = frequency + sumOfFrequencies t
    where
        (symbol,frequency) = h
  
{-
1. Trier la distribution des symboles rangée par ordre décroissant du
nombre d'apparitions
-}
shannonFano :: Ord a => [(a, Int)] -> Maybe (EncodingTree a)
shannonFano [] = Nothing
shannonFano freqs = Just $ buildTree $ sortFreqDesc freqs

sortFreqDesc :: [(a,Int)] -> [(a,Int)]
sortFreqDesc [] = [] -- Cas de base
sortFreqDesc (h:t) = sortFreqDesc below ++ [h] ++ sortFreqDesc above
  where below = filter (\y -> snd y >= snd h) t
        above = filter (\y -> snd y < snd h) t
{-
2. Couper en deux sous-distributions les plus équilibrées possibles
3. Recommencer l'étape 2 sur chaque sous-distribution jusqu'à obtenir
des distributions à un symbole
4. Construire l'arbre à partir des coupes
-}
buildTree :: [(a, Int)] -> EncodingTree a
buildTree [(sym, frequency)] = EncodingLeaf frequency sym
buildTree freqs =
    let (left, right) = splitEqually freqs
    in EncodingNode frequency (buildTree left) (buildTree right)
  where
    frequency = sumOfFrequencies freqs
{-
2. Couper en deux sous-distributions les plus équilibrées possibles
-}
splitEqually :: [(a, Int)] -> ([(a, Int)], [(a, Int)])
splitEqually [left,right] =  ([left],[right])
splitEqually freqs = splitEqually' freqs [] []
  where
    middle = sumOfFrequencies freqs `div` 2
    splitEqually' [] left right = (left, right)
    splitEqually' [(h,freq)] left right = (left ++ [(h,freq)], right) -- Cas de base pour le dernier élément unique
    splitEqually' ((h1,freq1):(h2,freq2):t) left right
      | sumOfFrequencies left + freq1 + freq2 >= middle = splitEqually'' ((h1,freq1):(h2,freq2):t) left right
      | otherwise = splitEqually' ((h2,freq2):t) (left ++ [(h1,freq1)]) (right)    

splitEqually'' :: [(a, Int)] -> [(a, Int)] -> [(a, Int)] -> ([(a, Int)], [(a, Int)])
splitEqually'' [] left right = (left, right)
splitEqually'' [(h,freq)] left right = (left ++ [(h,freq)], right) -- Cas de base pour le dernier élément unique
splitEqually'' ((h1,freq1):(h2,freq2):t) left right
  | diffBeforeMiddle - diffAfterMiddle > 0 = (left ++ [(h1,freq1)] ++ [(h2,freq2)], right ++ t) -- Si c'est positif, alors AfterMiddle est meilleur (SI LA LISTE EST DECROISSANTE)
  | otherwise = (left ++ [(h1,freq1)], right ++ [(h2,freq2)] ++ t)  -- Sinon, BeforeMiddle est meilleur (SI LA LISTE EST DECROISSANTE)
  where
    diffBeforeMiddle = abs(fromIntegral(sumOfFrequencies left + freq1) - middle)
    diffAfterMiddle = abs(fromIntegral(sumOfFrequencies left + freq1 + freq2) -middle)
    middle = divide (sumOfFrequencies t + sumOfFrequencies left + sumOfFrequencies right + freq1 + freq2)  2

-- Fait la division de 2 entiers et retourne un float
divide :: Int -> Int -> Float
divide x y = fromIntegral x / fromIntegral y

tree :: Ord a => [a] -> Maybe (EncodingTree a)
tree [] = Nothing
tree symbols = shannonFano $ frequencies symbols
