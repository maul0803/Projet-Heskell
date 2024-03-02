{- |
  Module : Statistic.ShannonFano
  Description : A module containing specifics for the Shannon-Fano compression method
  Maintainer : ???
-}
module Statistic.ShannonFano(tree) where

import Statistic.EncodingTree

-- | Shannon-Fano tree generation
tree :: Ord a => [a] -> Maybe (EncodingTree a)
tree _ = undefined -- TODO

