{-# LANGUAGE DeriveGeneric #-}

module World
  ( Span(..)
  , Layer
  , World
  , new
  , insert
  , UpMsg(..)
  , DownMsg(..)
  ) where

import Data.Aeson as Aeson
import Data.Char (toLower)
import GHC.Generics (Generic)
import Lens.Micro ((%~), (&), _head, ix)

data Span =
  Span
    { _x1 :: Float
    , _y1 :: Float
    , _x2 :: Float
    , _y2 :: Float
    }
  deriving (Show, Eq, Generic)

type Layer = [Span]

data Infinite a =
  Infinite
    { _bound :: Int
    , _list :: [a]
    }

instance Show a => Show (Infinite a) where
  show = show . bounded

data World =
  World
    { _back :: Infinite Layer
    , _fwd :: Infinite Layer
    }

instance Show World where
  show = show . serialize

data Serial =
  Serial
    { _origin :: Int
    , _layers :: [Layer]
    }
  deriving (Show, Generic)

data UpMsg =
  Insert
    { _layer :: Int
    , _span :: Span
    }
  deriving (Show, Generic)

data DownMsg =
  Update
    { _world :: World
    }
  deriving (Show, Generic)

new :: World
new = World empty singleton

insert :: Span -> Int -> World -> World
insert newSpan index world =
  if index < 0
    then world {_back = updateAt (-index - 1) (newSpan :) (_back world)}
    else world {_fwd = updateAt index (newSpan :) (_fwd world)}

serialize :: World -> Serial
serialize world =
  Serial {_origin = _bound $ _back world, _layers = (reverse $ bounded $ _back world) <> (bounded $ _fwd world)}

bounded :: Infinite a -> [a]
bounded (Infinite bound xs) = take bound xs

empty :: Monoid a => Infinite a
empty = Infinite 0 $ repeat mempty

singleton :: Monoid a => Infinite a
singleton = empty {_bound = 1}

updateAt :: Int -> (a -> a) -> Infinite a -> Infinite a
updateAt index update (Infinite bound xs) = Infinite (max bound $ index + 1) (xs & ix index %~ update)

-- JSON
instance ToJSON Span where
  toJSON = Aeson.genericToJSON recordOptions
  toEncoding = Aeson.genericToEncoding recordOptions

instance FromJSON Span where
  parseJSON = Aeson.genericParseJSON recordOptions

instance ToJSON Serial where
  toJSON = Aeson.genericToJSON recordOptions
  toEncoding = Aeson.genericToEncoding recordOptions

instance ToJSON World where
  toJSON = toJSON . serialize
  toEncoding = toEncoding . serialize

instance FromJSON UpMsg where
  parseJSON = Aeson.genericParseJSON msgOptions

instance ToJSON DownMsg where
  toJSON = Aeson.genericToJSON msgOptions
  toEncoding = Aeson.genericToEncoding msgOptions

recordOptions :: Aeson.Options
recordOptions = Aeson.defaultOptions {fieldLabelModifier = drop 1, constructorTagModifier = _head %~ toLower}

msgOptions :: Aeson.Options
msgOptions =
  recordOptions {tagSingleConstructors = True, sumEncoding = Aeson.defaultTaggedObject {tagFieldName = "msg"}}
