{-|
Module      :  Database.Persist.Migration.Utils.Plan
Maintainer  :  Brandon Chinn <brandonchinn178@gmail.com>
Stability   :  experimental
Portability :  portable

Define functions useful for compiling a plan of migration.
-}
{-# LANGUAGE TupleSections #-}

module Database.Persist.Migration.Utils.Plan
  ( getPath
  ) where

import Data.Graph.Inductive (Gr, mkGraph, sp)
import Data.HashMap.Lazy ((!))
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.IntSet as IntSet

type Node = Int
type Edge = (Int, Int)

-- | Given a list of edges and their data and a start/end node, return the shortest path.
--
-- Errors if no path is found.
getPath :: [(Edge, a)] -> Node -> Node -> Maybe [a]
getPath edgeData start end = map (edgeMap !) . nodesToEdges <$> sp start end graph
  where
    graph = mkGraph' $ map fst edgeData
    nodesToEdges nodes = zip nodes $ tail nodes
    edgeMap = HashMap.fromList edgeData

mkGraph' :: [Edge] -> Gr () Int
mkGraph' edgeData = mkGraph nodes edges
  where
    detuple (a, b) = [a, b]
    nodes = map (, ()) . IntSet.toList . IntSet.fromList . concatMap detuple $ edgeData
    edges = map (uncurry (,,1)) edgeData
