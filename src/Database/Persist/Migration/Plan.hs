{-|
Module      :  Database.Persist.Migration.Plan
Maintainer  :  Brandon Chinn <brandonchinn178@gmail.com>
Stability   :  experimental
Portability :  portable

Define functions useful for compiling a plan of migration.
-}
{-# LANGUAGE TupleSections #-}

module Database.Persist.Migration.Plan
  ( getPath
  ) where

import Data.Graph.Inductive (Gr, mkGraph, sp)
import Data.HashMap.Lazy ((!))
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.IntSet as IntSet
import Data.Maybe (fromJust)

type Node = Int
type Edge = (Int, Int)

-- | Given a list of edges and their data and a start/end node, return the shortest path.
--
-- Errors if no path is found.
getPath :: [(Edge, a)] -> Node -> Node -> [a]
getPath edgeData start end = map (edgeMap !) edgePath
  where
    edgeMap = HashMap.fromList edgeData
    edgePath = zip nodePath $ tail nodePath
    nodePath = fromJust . sp start end . mkGraph' $ map fst edgeData

mkGraph' :: [Edge] -> Gr () Int
mkGraph' edgeData = mkGraph nodes edges
  where
    detuple (a, b) = [a, b]
    nodes = map (, ()) . IntSet.toList . IntSet.fromList . concatMap detuple $ edgeData
    edges = map (uncurry (,,1)) edgeData
