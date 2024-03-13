{- |
  Module : Statistic.ShannonFano
  Description : A module containing specifics for the Shannon-Fano compression method
  Maintainer : ???
-}
module Statistic.ShannonFano(tree) where
import Statistic.EncodingTree ( EncodingTree(..) )
-- 1ère partie, trier la liste dans l'odre croissant de fréquence des symboles
--1. Trier la distribution des symboles rangée par ordre décroissant du
--nombre d'apparitions
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

-- Retourne le taille de la liste et la fréquence de chaque symboles
frequencies_and_total :: (Eq a) => [a] -> (Int,[(a, Int)])
frequencies_and_total [] = (0,[])
frequencies_and_total list = (length list,frequencies list)

-- Retourne le taille de la liste et la fréquence de chaque symboles dans l'ordre croissant
sorted_frequencies_and_total :: (Eq a) => [a] -> (Int,[(a, Int)])
sorted_frequencies_and_total list = (total,sorted_list)
    where (total,non_sorted_list) = frequencies_and_total list
          sorted_list = sortFreqAsc non_sorted_list

sortFreqDesc :: [(a,Int)] -> [(a,Int)]
sortFreqDesc [] = [] -- Cas de base
sortFreqDesc (h:t) = sortFreqDesc below ++ [h] ++ sortFreqDesc above
  where below = filter (\y -> snd y >= snd h) t
        above = filter (\y -> snd y < snd h) t

sortFreqAsc :: [(a,Int)] -> [(a,Int)]
sortFreqAsc [] = [] -- Cas de base
sortFreqAsc (h:t) = sortFreqAsc below ++ [h] ++ sortFreqAsc above
    where below = filter (\y -> snd y < snd h) t
          above = filter (\y -> snd y >= snd h) t

-- Seconde partie: Retourner 2 sublists
--2. Couper en deux sous-distributions les plus équilibrées possibles
-- Ce que je fais, c'est que j'obtiens les 2 listes les plus proches du millieu (avant de dépasser la limite et après avoir dépassé la limite), puis je les compare,
-- dans dividedList avec la variable diff
-- Retourne 2 listes, séparé selon la plus proche du millieu
divideList :: Int -> [(a,Int)] -> (Int,[(a,Int)],Int,[(a,Int)])
divideList _ [] = (0,[], 0,[])
divideList total list = dividedList total limit (listL_and_listR total limit list)
    where 
        limit = ceiling (fromIntegral total / 2)

-- Retourne les 2 listes les plus proche du millieu
listL_and_listR :: Int -> Int -> [(a,Int)] -> (Int, [(a,Int)], [(a,Int)],Int,[(a,Int)],[(a,Int)])
listL_and_listR _ _ [] = (0, [], [], 0, [], [])
listL_and_listR total limit list =
    case listL limit list of
        (lastLimit, extracted, []) -> (lastLimit, extracted, [], lastLimit, extracted, []) -- Pas forcément nécessaire car listL ne sera jamais vide, si on donne la liste vide, divideList renvoie le résultat avant
        (lastLimit, extracted, (symbol,freq_h):t) -> (lastLimit, extracted, (symbol,freq_h):t, lastLimit - freq_h, extracted ++ [(symbol,freq_h)], t)

-- Retourne la liste juste avec de dépasser le millieu (la plus proche depuis la gauche)
listL :: Int -> [(a,Int)] -> (Int, [(a,Int)], [(a,Int)])
listL _ [] = (0, [], [])
listL limit ((a,h):t)
    | h <= limit = (lastLimit, (a,h) : extracted, rest)
    | otherwise = (limit, [], (a,h):t)
    where
        (lastLimit, extracted, rest) = listL (if h <= limit then limit - h else limit) t

-- Prend en argument le total, la limite et la sortie de listL_and_listR
-- Retourne la liste qui est la plus proche du millieu (on veut la répartition la plus équilibrée)
dividedList :: Int -> Int -> (Int, [(a,Int)], [(a,Int)], Int, [(a,Int)], [(a,Int)]) -> (Int, [(a,Int)], Int, [(a,Int)])
-- Right est déja au plus proche de la limite, normalement n'est pas censé arriver car ca sera toujours left dans ce cas à cause de h <= limit = (lastLimit, (a,h) : extracted, rest)
dividedList total limit (_, _, _, 0, subList1_R, subList2_R) = (total1_R, subList1_R, total2_R, subList2_R)
    where
        total1_R = limit - total_R
        total2_R = if abs (limit - (total `div` 2)) == 0 then limit + total_R else limit + (total_R - 1)
        total_R = 0
-- Left est déja au plus proche de la limite
dividedList total limit (0, subList1_L, subList2_L, _, _, _) = (total1_L, subList1_L, total2_L, subList2_L)
    where
        total1_L = limit - total_L
        total2_L = if abs (limit - (total `div` 2)) == 0 then limit + total_L else limit + (total_L - 1)
        total_L = 0
dividedList total limit (total_L, subList1_L, subList2_L,total_R, subList1_R, subList2_R)
    | diff < 0 = (total1_R, subList1_R, total2_R, subList2_R)
    | otherwise = (total1_L, subList1_L, total2_L, subList2_L)
    where 
        diff = abs (total1_R - total2_R) - abs (total1_L - total2_L)
        total1_R = limit - total_R
        total2_R = if abs (limit - (total `div` 2)) == 0 then limit + total_R else limit + (total_R - 1)
        total1_L = limit - total_L
        total2_L = if abs (limit - (total `div` 2)) == 0 then limit + total_L else limit + (total_L - 1)

-- Partie main
createLeaf :: (Int,(a, Int)) -> EncodingTree a
createLeaf (freq, (symbol, _)) = EncodingLeaf freq symbol


--3. Recommencer l'étape 2 sur chaque sous-distribution jusqu'à obtenir
--des distributions à un symbole
--4. Construire l'arbre à partir des coupes
createTree :: (Int,[(a, Int)]) -> EncodingTree a
createTree (total, list)
    | length list == 1 = createLeaf (total, head list)
    | otherwise = EncodingNode total subTree1 subTree2
    where 
        (total1, list1,total2, list2)  = divideList total list
        total_and_sublist1 = (total1, list1)
        total_and_sublist2 = (total2, list2)
        subTree1 = createTree total_and_sublist1
        subTree2 = createTree total_and_sublist2

--4.Construire l'arbre à partir des coupes
tree :: Ord a => [a] -> Maybe (EncodingTree a)
tree text = Just (createTree total_and_list)
    where total_and_list = sorted_frequencies_and_total text
