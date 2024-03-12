{- |
  Module : Statistic.Huffman
  Description : A module containing specifics for the Huffman compression method
  Maintainer : ???
-}
module Statistic.Huffman(tree) where

import Statistic.EncodingTree

-- | Huffman tree generation
tree :: Ord a => [a] -> Maybe (EncodingTree a)
tree _ = undefined -- TODO
