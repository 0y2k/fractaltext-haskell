{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.FractalText.Encode where

import Data.FractalText.Serialize
import Data.FractalText.To

import Control.Monad ((>=>))
import Data.Bifunctor (first)
import qualified Data.Text.Lazy as TL
import Generic.Data

data EncodeError a
  = EncodeError'Serialize SerializeError
  | EncodeError'ToItem (ToItemError a)
  deriving (Generic)
instance Eq (ToItemError a) => Eq (EncodeError a) where
  (==) = geq
instance Show (ToItemError a) => Show (EncodeError a) where
  showsPrec = gshowsPrec

encode :: ToItem a => a -> Either (EncodeError a) TL.Text
encode = first EncodeError'ToItem . toItem
  >=> first EncodeError'Serialize . serializeNaked 2
