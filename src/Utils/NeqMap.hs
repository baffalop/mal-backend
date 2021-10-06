module Utils.NeqMap
  ( NeqMap
  , empty
  , toList
  , insert
  , delete
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

data NeqMap a =
  NeqMap
    { _index :: Int
    , _map :: Map Int a
    }

empty :: NeqMap a
empty = NeqMap 0 M.empty

toList :: NeqMap a -> [a]
toList (NeqMap _ map) = fmap snd $ M.toList map

insert :: a -> NeqMap a -> (NeqMap a, Int)
insert x (NeqMap index map) = (NeqMap (index + 1) (M.insert index x map), index)

delete :: Int -> NeqMap a -> NeqMap a
delete i map = map {_map = M.delete i $ _map map}
