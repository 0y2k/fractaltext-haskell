module Data.FractalText.Util where

import Data.FractalText.Type

import Control.Monad.Trans.Free
import Data.Functor (($>))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import Lens.Micro.Platform
import Prelude hiding (break, span)
import qualified Streaming as S
import qualified Streaming.Prelude as S

nextChar :: Monad m
         => S.Stream (S.Of T.Text) m r
         -> m (Either r (Char, S.Stream (S.Of T.Text) m r))
nextChar s0 = do
  mx <- S.next s0
  case mx of
    Left r -> return $ Left r
    Right (x, s1) -> case T.uncons x of
      Nothing -> nextChar s1
      Just (c, y) -> return $ Right (c, S.yield y >> s1)

span :: Monad m => (Char -> Bool) -> S.Stream (S.Of T.Text) m r
     -> S.Stream (S.Of T.Text) m (S.Stream (S.Of T.Text) m r)
span predicate = go
 where
  go s0 = do
    ex <- S.lift $ S.next s0
    case ex of
      Left r -> return $ return r
      Right (x, s1) -> do
        let (prefix, suffix) = T.span predicate x
        if T.null suffix
          then do
            S.yield x
            go s1
          else do
            S.yield prefix
            return $ S.yield suffix >> s1

break :: Monad m => (Char -> Bool) -> S.Stream (S.Of T.Text) m r
      -> S.Stream (S.Of T.Text) m (S.Stream (S.Of T.Text) m r)
break predicate = span (not . predicate)

splitLines :: Monad m
           => S.Stream (S.Of T.Text) m r
           -> FreeT (S.Stream (S.Of T.Text) m) m r
splitLines p0 = FreeT $ go1 p0
 where
  go0 r = return $ Free $ S.yield T.empty $> FreeT (return $ Pure r)
  go1 p1 = do
    ec <- S.next p1
    case ec of
      Left r -> go0 r
      Right (txt, p2) -> if T.null txt
        then go1 p2
        else return $ Free $ go2 $ S.yield txt >> p2
  go2 p1 = do
    p2 <- break (\c -> c == '\n' || c == '\r') p1
    return $ FreeT $ do
      ec1 <- nextChar p2
      case ec1 of
        Left r -> return $ Pure r
        Right ('\n', p3) -> go1 p3
        Right ('\r', p3) -> do
          ec2 <- nextChar p3
          case ec2 of
            Left r -> go0 r
            Right ('\n', p4) -> go1 p4
            Right (c, p4) -> go1 $ S.yield (T.singleton c) >> p4
        Right _ -> error "splitLines: unreachable"

freeTToLines :: Monad m => FreeT (S.Stream (S.Of T.Text) m) m r -> m [T.Text]
freeTToLines ft = do
  step <- runFreeT ft
  case step of
    Pure _ -> return []
    Free p0 -> do
      line S.:> p1 <- S.fold (<>) T.empty id p0
      rest <- freeTToLines p1
      return $ line : rest

fromLazyText :: Monad m => TL.Text -> S.Stream (S.Of T.Text) m ()
fromLazyText = TL.foldrChunks (\e a -> S.yield e >> a) (return ())

removeAnnotation :: ItemA -> Item
removeAnnotation ItemA'EmptyList = Item'EmptyList
removeAnnotation (ItemA'List _l es) = Item'List $ flip V.map es $ \e ->
  ListElem $ e ^. listElemA'value
removeAnnotation (ItemA'Dict _l des) = Item'Dict $ flip V.map des $ \de ->
  DictElem (de ^. dictElemA'key) (removeAnnotation $ de ^. dictElemA'value)

isValidDocument :: Document -> Bool
isValidDocument = go . fst
 where
  go ItemA'EmptyList = True
  go (ItemA'List _l es) = not $ V.null es
  go (ItemA'Dict _l des) = not (V.null des)
    && V.all (go . view dictElemA'value) des
