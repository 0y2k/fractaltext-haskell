module Data.FractalText.Annotate where

import Data.FractalText.Type
import Data.FractalText.Util

import Data.Functor.Identity (Identity(..))
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import Lens.Micro.Platform

containsNewline :: T.Text -> Bool
containsNewline = isJust . T.findIndex (== '\n')

shouldQuote :: T.Text -> Bool
shouldQuote ts = T.null ts || case T.unpack ts of
  ' ':_ -> True
  ':':_ -> True
  '\"':ts1 -> case reverse ts1 of
    '\"':_ -> True
    _ -> False
  _ -> False

annotate :: Int -> Item -> ItemA
annotate isucc = go 0
 where
  h ts = ListElemA V.empty (shouldQuote ts) ts
  go _i Item'EmptyList = ItemA'EmptyList
  go i (Item'List vs) =
    let f (ListElem ts)
          | containsNewline ts = V.fromList $ map h
            $ runIdentity $ freeTToLines
            $ splitLines $ fromLazyText $ TL.fromStrict ts
          | otherwise = V.singleton $ h ts
     in ItemA'List i $ V.concatMap f vs
  go i (Item'Dict des) =
    let f de = DictElemA V.empty
          (de ^. dictElem'key)
          (go (i + isucc) $ de ^. dictElem'value)
     in ItemA'Dict i $ V.map f des
