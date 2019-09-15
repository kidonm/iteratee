{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-} -- to avoid name clashes

import Prelude (Monad, (.), ($), return, Maybe(Just, Nothing), fmap, (>>=), error, Int, undefined, Functor, Applicative, pure,(<*>), (+), IO, print, read, getLine, (==), putStrLn)
import Control.Monad (liftM, ap, (>>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.Maybe (maybe)

data Stream a
  = Chunk [a]
  | Eof

data Step a m b
  = Yield b (Stream a)
  | Continue (Stream a -> Iteratee a m b)

newtype Iteratee a m b = Iteratee { runIteratee :: (m (Step a m b)) }

type Enumerator a m b = Step a m b -> Iteratee a m b

type Enumeratee a a' m b = Step a m b -> Iteratee a' m (Step a m b)

instance Monad m => Functor (Iteratee a m) where
  fmap = liftM

instance Monad m => Applicative (Iteratee a m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (Iteratee a m) where
  return b = yield b (Chunk [])

  i >>= f = Iteratee $ runIteratee i >>= \case
    Continue k -> return $ Continue $ \s -> k s >>= f
    Yield a (Chunk []) -> runIteratee $ f a
    Yield a extra -> runIteratee (f a) >>= \case
      Yield a' _ -> pure $ Yield a' extra
      Continue k -> runIteratee $ k extra

instance (MonadIO m) => MonadIO (Iteratee a m) where
  liftIO m = Iteratee $ (liftIO m) >>= runIteratee . return

instance MonadTrans (Iteratee a) where
  lift m = Iteratee $ m >>= runIteratee . return

pureI :: (Monad m) => Step a m b -> Iteratee a m b
pureI = Iteratee . return

yield :: (Monad m) => b -> Stream a -> Iteratee a m b
yield b s = Iteratee $ return $ Yield b s

continue :: (Monad m) => (Stream a -> Iteratee a m b) -> Iteratee a m b
continue = Iteratee . return . Continue

head :: (Monad m) => Iteratee a m (Maybe a)
head = continue go
  where
    go = \case
      Chunk (a:aa) -> yield (Just a) (Chunk aa)
      Chunk [] -> continue go
      Eof -> yield Nothing Eof

sum :: (Monad m) => Iteratee Int m Int
sum = head >>= maybe (return 0) add
  where
    add i = do
      rest <- sum
      return $ i + rest

run :: (Monad m) => Iteratee a m b -> m b
run (Iteratee mstep) = mstep >>= go
  where
    go = \case
      Yield b _ -> return b
      Continue k -> runIteratee (k Eof) >>= \case
        Continue _ -> error "Iteratee SHOULD NOT continue after Eof"
        Yield b _ -> return b

enum :: Monad m => Enumerator a m b -> Iteratee a m b -> Iteratee a m b
enum e (Iteratee mstep) = Iteratee $ mstep >>= runIteratee . e

feedChunk :: (Monad m) => [a] -> Enumerator a m a
feedChunk input (Continue k) = k $ Chunk input
feedChunk _ step = pureI step

infixl 1 >>==
(>>==) :: (Monad m) => Iteratee a m b -> (Step a m b -> Iteratee a' m b') -> Iteratee a' m b'
i >>== f = Iteratee $ runIteratee i >>= runIteratee . f

getNumber :: Enumerator Int IO b
getNumber (Continue k) = do
  line <- liftIO $ putStrLn "Insert a number, press 'q' to exit:" >> getLine
  if line == "q"
  then continue k
  else (k (Chunk [read line])) >>== getNumber
getNumber step = pureI step

-- type Enumeratee a a' m b = Step a m b -> Iteratee a' m (Step a m b)

skip :: Monad m => Enumeratee a a m b
skip (Continue k) = do
  x <- head
  _ <- head
  maybe (return $ Continue k) onJust x
  where
    onJust y = do
      newStep <- lift $ runIteratee $ k $ Chunk [y]
      skip newStep
skip step = return step

joinI :: Monad m => Iteratee a m (Step a' m b) -> Iteratee a m b
joinI outer = do
  inner <- outer
  go inner
  where
    go = \case
      Continue k -> k Eof >>== go
      Yield x _ -> yield x Eof

test1 = run sum >>= print

test2 = run program >>= print
  where
    program = enum (feedChunk [0..9]) sum

test3 = run program >>= print
  where
    program = enum (feedChunk [0..9]) $ enum (feedChunk [10..19]) sum

test4 = run program >>= print
  where
    program = enum getNumber sum

test5 = run program >>= print
  where
    program = enum getNumber $ joinI $ sum >>== skip

