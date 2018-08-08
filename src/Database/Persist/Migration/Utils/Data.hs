{-|
Module      :  Database.Persist.Migration.Utils.Data
Maintainer  :  Brandon Chinn <brandonchinn178@gmail.com>
Stability   :  experimental
Portability :  portable

Define functions useful for data constructors.
-}

module Database.Persist.Migration.Utils.Data
  ( showConstr
  , isConstr
  , hasDuplicateConstrs
  ) where

import Data.Data (Data)
import qualified Data.Data as Data
import Data.Function (on)
import Data.List (nubBy)

-- | Show the name of the constructor.
showConstr :: Data a => a -> String
showConstr = Data.showConstr . Data.toConstr

-- | Return True if the given constructor has the given name.
isConstr :: Data a => String -> a -> Bool
isConstr name = (== name) . showConstr

-- | Return True if the given list has duplicate constructors.
hasDuplicateConstrs :: Data a => [a] -> Bool
hasDuplicateConstrs l = length l /= length (nubBy ((==) `on` showConstr) l)
