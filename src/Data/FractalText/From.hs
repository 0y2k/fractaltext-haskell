{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.FractalText.From
  ( FromItem(..)
  , FromKey(..)
  , FromItemError'Common(..)
  , FromItemError'Map(..)
  ) where

import Data.FractalText.Type

import Control.Monad (forM)
import Data.Bifunctor (Bifunctor(first))
import Data.Kind (Type)
import qualified Data.Map as M
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Short as ST
import qualified Data.Vector as V
import Data.Void (Void)
import Generic.Data
import Lens.Micro.Platform

class FromItem a where
  type FromItemError a :: Type
  fromItem :: Item -> Either (FromItemError a) a
class Ord a => FromKey a where
  type FromKeyError a :: Type
  fromKey :: Key -> Either (FromKeyError a) a

instance FromItem Item where
  type FromItemError Item = Void
  fromItem = Right

data FromItemError'Common
  = FromItemError'Common'InvalidValue
  deriving (Generic, Eq, Show)

instance FromItem TS.Text where
  type FromItemError TS.Text = FromItemError'Common
  fromItem (Item'List vs)
    | V.length vs == 1 = Right $ V.head vs ^. listElem'value
    | otherwise = Left FromItemError'Common'InvalidValue
  fromItem _ = Left FromItemError'Common'InvalidValue
instance FromItem TL.Text where
  type FromItemError TL.Text = FromItemError'Common
  fromItem (Item'List vs)
    | V.length vs == 1 = Right $ TL.fromStrict $ V.head vs ^. listElem'value
    | otherwise = Left FromItemError'Common'InvalidValue
  fromItem _ = Left FromItemError'Common'InvalidValue

instance a ~ TS.Text => FromItem [a] where
  type FromItemError [a] = FromItemError'Common
  fromItem (Item'List xs) = Right $ V.toList $ V.map (view listElem'value) xs
  fromItem _ = Left FromItemError'Common'InvalidValue
instance a ~ TS.Text => FromItem (V.Vector a) where
  type FromItemError (V.Vector a) = FromItemError'Common
  fromItem (Item'List xs) = Right $ V.map (view listElem'value) xs
  fromItem _ = Left FromItemError'Common'InvalidValue

instance FromKey TS.Text where
  type FromKeyError TS.Text = Void
  fromKey st = Right $ ST.toText st
instance FromKey TL.Text where
  type FromKeyError TL.Text = Void
  fromKey st = Right $ TL.fromStrict $ ST.toText st
instance FromKey ST.ShortText where
  type FromKeyError ST.ShortText = Void
  fromKey = Right

data FromItemError'Map k v
  = FromItemError'Map'InvalidValue
  | FromItemError'Map'KeyError (FromKeyError k)
  | FromItemError'Map'ValueError (FromItemError v)
  deriving (Generic)
instance (Eq (FromKeyError k), Eq (FromItemError v))
  => Eq (FromItemError'Map k v) where
    (==) = geq
instance (Show (FromKeyError k), Show (FromItemError v))
  => Show (FromItemError'Map k v) where
    showsPrec = gshowsPrec

instance (FromKey k, FromItem v) => FromItem (M.Map k v) where
  type FromItemError (M.Map k v) = FromItemError'Map k v
  fromItem (Item'Dict dic) =
    fmap M.fromList $ forM (V.toList dic) $ \de -> do
      kd <- first FromItemError'Map'KeyError $ fromKey $ de ^. dictElem'key
      vd <- first FromItemError'Map'ValueError $ fromItem $ de ^. dictElem'value
      return (kd, vd)
  fromItem _ = Left FromItemError'Map'InvalidValue
