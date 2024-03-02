{- |
  Module      : RLE
  Description : An implementation of the run-length encoding method
  Maintainer  : ???
-}
module RLE(compress, uncompress) where

-- | RLE compress method
compress :: Eq a => [a] -> [(a, Int)]
compress [] = []
compress (x:xs) = compress' xs x 1  -- Sépare le premier élément x et le reste de la liste xs, en initialisant le compteur de caractères à 1
  where
    compress' [] c count = [(c, count)]  -- Cas de base de compress, si la liste est vide, on renvoie le résultat
    compress' (x:xs) c count
      | x == c = compress' xs c (count + 1) -- 1er cas, le caractere suivant est le même que celui d'avant, on incrémente le compteur de 1
      | x /= c = (c, count) : compress' xs x 1 --2ème cas, le caractere suivant n'est pas le même que celui d'avant, on 
      -- ajouter (c, count) à la liste compress et on contisnue avec le nouveau caractere (otherwise pourrait être utilisé)


-- | RLE uncompress method
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: [(a, Int)] -> Maybe [a]
uncompress = foldl uncompressOne (Just []) -- On applique la fonction uncompressOne de gauche à droite en intialisat avec []
  where
    uncompressOne :: Maybe [a] -> (a, Int) -> Maybe [a]  -- Peut renvoyer Nothing si la décompression échoe (ex: ['a',0])
    uncompressOne _ (c, count)
      | count < 1 = Nothing -- Cas de la décompression impossible
    uncompressOne (Just acc) (c, count) = Just (acc ++ replicate count c)
    uncompressOne Nothing _ = Nothing