{- |
  Module : Statistic.Source
  Description : Some utility functions for sources (input messages)
  Maintainer : ???
-}

module Statistic.Source(occurrences, entropy, orderedCounts) where

import Data.Map (Map)

-- | The map giving occurrences of each symbol in the source
occurrences :: Ord a => [a] -> Map a Int
occurrences _ = undefined -- TODO

-- | SHANNON entropy of source
entropy :: Ord a => [a] -> Double
entropy _ = undefined -- TODO

-- | List of occurrences ordered by count
orderedCounts :: Ord a => [a] -> [(a, Int)]
orderedCounts _ = undefined -- TODO

