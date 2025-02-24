{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.FractalText.Type where

import qualified Data.Text as T
import qualified Data.Text.Short as ST
import qualified Data.Vector as V
import Generic.Data
import Lens.Micro.Platform

type Key = ST.ShortText

newtype ListElem a = ListElem
  { _listElem'value :: a
  }
  deriving (Generic, Eq, Functor, Show)

data DictElem a = DictElem
  { _dictElem'key :: Key
  , _dictElem'value :: a
  }
  deriving (Generic, Eq, Functor, Show)

data Item
  = Item'EmptyList
  | Item'List (V.Vector (ListElem T.Text))
  | Item'Dict (V.Vector (DictElem Item))
  deriving (Generic, Eq, Show)

data Line
  = Line'Blank
  | Line'Comment Int T.Text
  | Line'Key Int Key
  | Line'String Int T.Text
  deriving (Generic, Eq, Show)

data SurplusLine
  = SurplusLine'Blank
  | SurplusLine'Comment Int T.Text
  deriving (Generic, Eq, Show)

type Quoted = Bool

data ListElemA a = ListElemA
  { _listElemA'surplusLines :: V.Vector SurplusLine
  , _listElemA'quoted :: Bool
  , _listElemA'value :: a
  }
  deriving (Generic, Eq, Functor, Show)

data DictElemA a = DictElemA
  { _dictElemA'surplusLines :: V.Vector SurplusLine
  , _dictElemA'key :: Key
  , _dictElemA'value :: a
  }
  deriving (Generic, Eq, Functor, Show)

data ItemA
  = ItemA'EmptyList
  | ItemA'List Int (V.Vector (ListElemA T.Text))
  | ItemA'Dict Int (V.Vector (DictElemA ItemA))
  deriving (Generic, Eq, Show)

type Document = (ItemA, V.Vector SurplusLine)

$(makeLenses ''ListElem)
$(makeLenses ''DictElem)
$(makeLenses ''ListElemA)
$(makeLenses ''DictElemA)
