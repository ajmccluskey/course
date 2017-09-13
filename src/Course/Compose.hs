{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.Monad
import Course.Comonad (Comonad (..))

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a =
  Compose (f (g a))

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) =>
    Functor (Compose f g) where
  (<$>) f (Compose x) =
    Compose $ (f <$>) <$>  x

instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where
-- Implement the pure function for an Applicative instance for Compose
  pure =
    Compose . pure . pure
-- Implement the (<*>) function for an Applicative instance for Compose
  (<*>) (Compose fgh) (Compose fga) =
    Compose $ pure (<*>) <*> fgh <*> fga

-- Some type signatures/expressions to help explain the above to students
-- gh :: g (a -> b)
-- ga :: g a
-- gh <*> ga :: g b
-- pure (,) <*> Full 3 <*> Full "foo" :: Optional (Int, String)
-- pure (\gh ga -> gh <*> ga) <*> fgh <*> fga :: f (g b)
-- pure (<*>) <*> fgh <*> fga

instance (Monad f, Comonad f, Monad g) =>
  Monad (Compose f g) where
-- Implement the (=<<) function for a Monad instance for Compose
  (=<<) f (Compose fga) =
    let foo ga = pure $ ((\(Compose fgb) -> copure fgb) . f) =<< ga
     in Compose (foo =<< fga)

-- We can't implement the above with only Monad constraints, because the function
-- argument to bind adds structure that we don't know how to remove.
--
-- * `f` is the only way we can get an `a` to a `b`
-- * `f :: a -> Compose f g b`
-- * As soon as we bind through `g a`, we can't return `g b` because all
--   we can do is return a `Compose f g b`, and we don't know how to peel
--   off the `f`.
--
-- Note above that we _can_ implement `Compose` when our outer structure (`f`)
-- is also an instance of `Comonad`, as that allows us to peel off enough of
-- the structure to bind over `g a` and return a `g b`.
