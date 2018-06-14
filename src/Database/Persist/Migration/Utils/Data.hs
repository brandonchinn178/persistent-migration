{-|
Module      :  Database.Persist.Migration.Utils.Data
Maintainer  :  Brandon Chinn <brandonchinn178@gmail.com>
Stability   :  experimental
Portability :  portable

Define functions useful for data constructors.
-}

module Database.Persist.Migration.Utils.Data
  ( hasDuplicateConstrs
  ) where

import Data.Data (Data, showConstr, toConstr)
import Data.Function (on)
import Data.List (nubBy)

-- | Show the name of the constructor.
showData :: Data a => a -> String
showData = showConstr . toConstr

-- | Return True if the given list has duplicate constructors.
hasDuplicateConstrs :: Data a => [a] -> Bool
hasDuplicateConstrs l = length l /= length (nubBy ((==) `on` showData) l)
