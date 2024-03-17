{- |
  Module : Statistic.Source
  Description : Some utility functions for sources (input messages)
  Maintainer : ???
-}

module Statistic.Source(occurrences, entropy, orderedCounts) where

import qualified Data.Map as Map
import Data.Map ((!))
import Data.Maybe (fromMaybe)
import Data.List (sortBy)
import Data.Ord (comparing)

-- | The map giving occurrences of each symbol in the source
occurrences :: Ord a => [a] -> Map.Map a Int
occurrences = foldr (\symbol -> Map.insertWith (+) symbol 1) Map.empty

-- | SHANNON entropy of source
entropy :: Ord a => [a] -> Double
entropy source =
    let occs = occurrences source
        total = fromIntegral $ sum (Map.elems occs)
        probs = Map.map (\occ -> let prob = fromIntegral occ / total in prob * logBase 2 prob) occs
    in negate $ sum (Map.elems probs)

-- | List of occurrences ordered by count
orderedCounts :: Ord a => [a] -> [(a, Int)]
orderedCounts = sortBy (comparing snd) . Map.toList . occurrences
