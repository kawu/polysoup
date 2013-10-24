-- | A generic extracting predicate.


module Text.XML.PolySoup.Predicate
( Q (..)
, true
, satisfy
) where


import           Control.Applicative


-- | A predicate checks if the given element satisfies some properties
-- and extracts its attribute values.  You can compose predicates using
-- Functor, Applicative and Alternative operators: '*>', '<*', '<|>' etc.
--
-- It doesn't make sense to use `many` or `some` on the predicate.
-- Point it out!
newtype Q a b = Q { runQ :: (a -> Maybe b) }

instance Functor (Q a) where
    fmap f (Q g) = Q $ fmap (fmap f) g

instance Applicative (Q a) where  
    pure = Q . const . Just
    Q f <*> Q p = Q $ \t -> f t <*> p t

instance Alternative (Q a) where
    empty = Q $ \_ -> Nothing
    Q p <|> Q p' = Q $ \t -> p t <|> p' t


-- | Predicate which is always satisfied.
true :: Q a a
true = Q Just


-- | Check if the given predicate is satisfied.
satisfy :: (a -> Bool) -> Q a a
satisfy p = Q $ \t -> if p t
    then Just t
    else Nothing
