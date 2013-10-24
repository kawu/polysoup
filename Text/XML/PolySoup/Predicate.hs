-- | A generic extracting predicate.


module Text.XML.PolySoup.Predicate
( P (..)
, any
, satisfy
) where


import           Prelude hiding (any)
import           Control.Applicative


-- | A predicate checks if the given element satisfies some properties
-- and extracts its attribute values.  You can compose predicates using
-- Functor, Applicative and Alternative operators: '*>', '<*', '<|>' etc.
--
-- It doesn't make sense to use `many` or `some` on the predicate.
-- Point it out!
newtype P a b = P { runP :: (a -> Maybe b) }

instance Functor (P a) where
    fmap f (P g) = P $ fmap (fmap f) g

instance Applicative (P a) where  
    pure = P . const . Just
    P f <*> P p = P $ \t -> f t <*> p t

instance Alternative (P a) where
    empty = P $ \_ -> Nothing
    P p <|> P p' = P $ \t -> p t <|> p' t


-- | Predicate which is always satisfied.
any :: P a a
any = P Just


-- | Check if the given predicate is satisfied.
satisfy :: (a -> Bool) -> P a a
satisfy p = P $ \t -> if p t
    then Just t
    else Nothing
