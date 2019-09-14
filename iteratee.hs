{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Control.Monad ((>=>), forever)
import Data.Functor.Identity

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

readLnEnum :: Enumerator String IO a
readLnEnum (GetC f) = readLn >>= f
readLnEnum a = pure a

enumWords :: (Monad m) => Enumeratee Char String m a
enumWords (Done a) = pure $ Done a
enumWords (GetC f) = loop ""
  where
    loop :: String -> I Char m (I string m a)
    loop acc = undefined

-- infixr 1 .|
-- (.|) :: (Monad m) => (I b m a -> w) -> I b m (I c m a) -> w
-- i .| ii = undefined

-- getLine0 :: Source IO String
-- getLine0 = liftSource readLn

-- getLines0 :: (Monad m) => I String m [String]
-- getLines0 = GetC $ \s -> s:getLines0

-- take0 :: (Monad m) => I String m [String]
-- take0 = undefined
--
-- pipe :: I b m a -> I a m c -> I b m c
-- pipe () () =
--
-- test = getLine0 `pipe` take0

-- I Void m [String]

-- eval :: (Monad m) => [b] -> I b m a -> m a
-- eval "" (GetC f) = f Nothing >>= eval ""
-- eval _ (Done a) = pure a
-- eval (s:ss) (GetC f) = f (Just s) >>= eval ss

-- test = eval "bla\nbla bla\n" i

-- i :: IPure (String, String)
-- i = (,) <$> getLine0 <*> getLine0
