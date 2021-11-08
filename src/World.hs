{-# LANGUAGE DeriveGeneric #-}

module World
  ( Span(..)
  , Layer
  , World
  , new
  , insert
  , UpMsg(..)
  , DownMsg(..)
  , Serial(..)
  , serialize
  ) where

import Data.Aeson as Aeson
import Data.Char (toLower)
import Data.Default (Default, def)
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

data Extensible a =
  Extensible
    { _bound :: Int
    , _list :: [a]
    }

instance Show a => Show (Extensible a) where
  show = show . bounded

data World =
  World
    { _back :: Extensible Layer
    , _fwd :: Extensible Layer
    }

instance Show World where
  show = show . serialize

data Serial =
  Serial
    { _origin :: Int
    , _layers :: [Layer]
    }
  deriving (Show, Generic, Eq)

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

bounded :: Extensible a -> [a]
bounded (Extensible bound xs) = take bound xs

empty :: Default a => Extensible a
empty = Extensible 0 $ repeat def

singleton :: Default a => Extensible a
singleton = empty {_bound = 1}

updateAt :: Int -> (a -> a) -> Extensible a -> Extensible a
updateAt index update (Extensible bound xs) = Extensible (max bound $ index + 1) (xs & ix index %~ update)

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
