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
    Q f <*> Q p = Q $ \x -> f x <*> p x

instance Alternative (Q a) where
    empty = Q $ \_ -> Nothing
    Q p <|> Q p' = Q $ \x -> p x <|> p' x

-- Is there really sense in defining the Monad instance here?
-- Order of operations doesn't mean anything here, I suppose?
-- Well, it has influence on when the extraction stops when
-- one of the predicates is not satisfied. 
--
-- On the other hand, it may be better if it's obvious that a
-- monadic code means parsing here.
-- instance Monad (Q a) where
--     return = pure
--     Q p >>= f = Q $ \x -> do
--         y <- p x
--         runQ (f y) x


-- | Predicate which is always satisfied.
true :: Q a a
true = Q Just


-- | Check if the given predicate is satisfied.
satisfy :: (a -> Bool) -> Q a a
satisfy p = Q $ \t -> if p t
    then Just t
    else Nothing
