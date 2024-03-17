{- |
  Module      : RLE
  Description : An implementation of the run-length encoding method
  Maintainer  : ???
-}
module RLE(compress, uncompress) where

-- | RLE compress method
compress :: Eq a => [a] -> [(a, Int)]
compress [] = []
compress (h:t) = compressRec t h 1  -- Sépare le premier élément h et le reste de la liste t, en initialisant le compteur de caractères à 1

compressRec :: Eq a => [a] -> a -> Int -> [(a, Int)]
compressRec [] symbol acc = [(symbol, acc)]
compressRec (h:t) symbol acc
  | h == symbol = compressRec t symbol (acc + 1) -- Si le nouveau symbole h est le même que l'ancien symbole symbol, on ajoute 1 à l'accumulateur
  | otherwise = (symbol, acc) : compressRec t h 1 -- Si le nouveau symbole h n'est pas le même que l'ancien symbole symbol, on appelle récursivement compressRec avec un accumulateur initialisé à 1


-- | RLE uncompress method
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: [(a, Int)] -> Maybe [a]
uncompress = foldl uncompressOne (Just []) -- On applique la fonction uncompressOne de gauche à droite en intialisat avec []

uncompressOne :: Maybe [a] -> (a, Int) -> Maybe [a]  -- Peut renvoyer Nothing si la décompression échoe (ex: ['a',0])
uncompressOne _ (symbol, count)
  | count < 1 = Nothing -- Cas de la décompression impossible
uncompressOne (Just acc) (symbol, count) = Just (acc ++ replicate count symbol) -- replicate 2 'x' -> ['x','x'] (replicate :: Int -> a -> [a])
uncompressOne Nothing _ = Nothing