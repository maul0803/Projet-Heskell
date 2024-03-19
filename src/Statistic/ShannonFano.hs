{- |
  Module : Statistic.ShannonFano
  Description : A module containing specifics for the Shannon-Fano compression method
  Maintainer : ???
-}
module Statistic.ShannonFano(tree) where
import Statistic.EncodingTree ( EncodingTree(..) )
import Data.Ord (comparing, Down(..))
import Data.List (sortBy)


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
-- éviter de calculer plusieurs fois la fréquence du même élément
frequencies :: (Eq a) => [a] -> [(a, Int)]
frequencies xs = map (\x -> (x, length . filter (== x) $ xs)) . unique $ xs
  where unique [] = []
        unique (y:ys) = y : unique (filter (/=y) ys)

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

sortFreqDesc :: [(a,Int)] -> [(a,Int)]
sortFreqDesc = sortBy (comparing (Down . snd))

{-
2. Couper en deux sous-distributions les plus équilibrées possibles
3. Recommencer l'étape 2 sur chaque sous-distribution jusqu'à obtenir
des distributions à un symbole
4. Construire l'arbre à partir des coupes
-}
-- dans le cas simple d'un seul symbole, on créé deux branches pour s'assurer que l'arbre n'est pas dégénéré.
buildTree :: [(a, Int)] -> EncodingTree a
buildTree xs
  | length xs == 1 = let [(sym, freq)] = xs in EncodingNode freq (EncodingLeaf freq sym) (EncodingLeaf freq sym) -- Crée deux branches pour un seul symbole
  | otherwise = let (left, right) = splitEqually xs in EncodingNode (sumFreq xs) (buildTree left) (buildTree right)
  where
    sumFreq = sum . map snd

{-
2. Couper en deux sous-distributions les plus équilibrées possibles
-}

splitEqually :: [(a, Int)] -> ([(a, Int)], [(a, Int)])
splitEqually freqs = splitAt (length freqs `div` 2) $ sortBy (comparing snd) freqs

splitEqually'' :: [(a, Int)] -> [(a, Int)] -> [(a, Int)] -> ([(a, Int)], [(a, Int)])
splitEqually'' [] left right = (left, right)
splitEqually'' [(h,freq)] left right = (left ++ [(h,freq)], right) -- Cas de base pour le dernier élément unique
splitEqually'' ((h1,freq1):(h2,freq2):t) left right
  | diffBeforeMiddle - diffAfterMiddle > 0 = (left ++ [(h1,freq1)] ++ [(h2,freq2)], right ++ t) -- Si c'est positif, alors AfterMiddle est meilleur (SI LA LISTE EST DECROISSANTE)
  | otherwise = (left ++ [(h1,freq1)], right ++ [(h2,freq2)] ++ t)  -- Sinon, BeforeMiddle est meilleur (SI LA LISTE EST DECROISSANTE)
  where
    diffBeforeMiddle = abs((sumOfFrequencies left + freq1) - (sumOfFrequencies t + sumOfFrequencies right + freq2)) 
    diffAfterMiddle = abs((sumOfFrequencies left + freq1 + freq2) - (sumOfFrequencies t + sumOfFrequencies right))

-- Fait la division de 2 entiers et retourne un float
divide :: Int -> Int -> Float
divide x y = fromIntegral x / fromIntegral y

tree :: Ord a => [a] -> Maybe (EncodingTree a)
tree [] = Nothing
tree symbols = Just . buildTree . sortFreqDesc . frequencies $ symbols
