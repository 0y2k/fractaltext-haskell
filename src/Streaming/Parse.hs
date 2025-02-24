module Streaming.Parse where

import Control.Monad.Trans.Free
import qualified Control.Monad.Trans.State.Strict as State
import qualified Streaming as S
import qualified Streaming.Prelude as S

type Parser a b m r = State.StateT (S.Stream (S.Of a) m b) m r

draw :: Monad m => Parser a b m (Maybe a)
draw = do
  s0 <- State.get
  ex <- S.lift $ S.next s0
  case ex of
    Left r -> do
      State.put $ return r
      return Nothing
    Right (a, s1) -> do
      State.put s1
      return $ Just a

unDraw :: Monad m => a -> Parser a b m ()
unDraw a = State.modify $ S.cons a

peek :: Monad m => Parser a b m (Maybe a)
peek = do
  ma <- draw
  mapM_ unDraw ma
  return ma

isEndOfInput :: Monad m => Parser a b m Bool
isEndOfInput = do
  ma <- peek
  return $ case ma of { Nothing -> True; Just _ -> False }

folds :: Monad m => (x -> a -> x) -> x -> (x -> b)
      -> FreeT (S.Stream (S.Of a) m) m r
      -> S.Stream (S.Of b) m r
folds step begin done = go
 where
  go f0 = do
    x <- S.lift $ runFreeT f0
    case x of
      Pure r -> return r
      Free s -> do
        (f1, b) <- S.lift $ fold s begin
        S.yield b
        go f1
  fold s0 x = do
    ea <- S.next s0
    case ea of
      Left f -> return (f, done x)
      Right (a, s1) -> fold s1 $! step x a
