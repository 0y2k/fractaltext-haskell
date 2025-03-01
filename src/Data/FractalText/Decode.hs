{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.FractalText.Decode where

import Data.FractalText.From
import Data.FractalText.Parse

import Control.Monad ((>=>))
import Data.Bifunctor (first)
import qualified Data.Text.Lazy as TL
import Generic.Data

data DecodeError a
  = DecodeError'Parse ParseError
  | DecodeError'FromItem (FromItemError a)
  deriving (Generic)
instance Eq (FromItemError a) => Eq (DecodeError a) where
  (==) = geq
instance Show (FromItemError a) => Show (DecodeError a) where
  showsPrec = gshowsPrec

decode :: FromItem a => TL.Text -> Either (DecodeError a) a
decode = first DecodeError'Parse . parseNaked
  >=> first DecodeError'FromItem . fromItem
