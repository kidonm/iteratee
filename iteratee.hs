{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Control.Monad ((>=>), forever)
import Data.Functor.Identity
import Data.Char (isSpace)

data Void

type LChar = Maybe Char

data I b (m :: * -> *) a
  = Done a
  | GetC (b -> m (I b m a))

type Enumerator b m a = I b m a -> m (I b m a)

type Enumeratee b b' m a = I b' m a -> I b m (I b' m a)

instance (Functor m) => Functor (I b m) where
  fmap f (Done a) = Done $ f a
  fmap f (GetC c) = GetC $ \c' -> fmap (fmap f ) (c c')

instance Applicative m => Applicative (I b m) where
  pure = Done

  Done f <*> i = fmap f i
  GetC f <*> a = GetC $ \c -> (<*> a) <$> f c

instance Monad m => Monad (I b m) where
  return = pure
  Done a >>= f = f a
  GetC f >>= g = GetC $ fmap (>>= g) . f

run :: (Monad m) => [b] -> I (Maybe b) m a -> m a
run _ (Done a) = pure a
run (b:bs) (GetC f) = f (Just b) >>= run bs
run [] (GetC f) = f Nothing >>= run []

count :: (Monad m) => I (Maybe b) m Int
count = go 0
  where
    go n = GetC $ return . \case
      Just a -> go (n + 1)
      Nothing -> pure $ n

enumWords :: (Monad m) => Enumeratee Char String m a
enumWords (Done a) = pure $ Done a
enumWords (GetC f) = loop ""
  where
    loop acc = GetC $ check acc
    check acc ' ' = pure <$> f (reverse acc)
    check acc s = pure $ loop (s:acc)

filterEnum :: (Monad m) => (b -> Bool) -> Enumeratee (Maybe b) (Maybe b) m a
filterEnum pred (Done a) = pure $ Done a
filterEnum pred (GetC f) = go
  where
    go = GetC inner
    inner = \case
      Just b -> if pred b then pure <$> f (Just b) else pure go
      Nothing -> pure go

takeEnum :: (Monad m) => Int -> Enumeratee b b m a
takeEnum _ (Done a) = pure $ Done a
takeEnum n (GetC f) = undefined
  -- where
  --   go :: Int -> I b m (I b m a)
  --   go n
  --     | n > 1 = GetC $ \b -> (f b) >>= _
  --     | otherwise = undefined

infixr 1 .|
(.|) :: (Monad m) => (I b' m a -> w) -> I b m (I b' m a) -> w
fi .| (Done i) = fi i
fi .| (GetC f) = fi .| (GetC f)

pair :: (Monad m) => I b m a -> I b m c -> I b m (a, c)
pair (Done a) (Done c) = pure (a, c)
pair (GetC fa) (GetC fc) = GetC $ \b -> pair <$> fa b <*> fc b
pair (GetC fa) ic = GetC $ fmap (flip pair ic) . fa
pair ia (GetC fc) = GetC $ fmap (pair ia) . fc


