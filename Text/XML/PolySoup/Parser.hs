{-# LANGUAGE TupleSections #-}


-- | A generic parser.


module Text.XML.PolySoup.Parser
(
-- * Core
  P (..)
, evalP

-- * Parsing
-- ** Sequential
, one
-- ** Selective
, first
, every
) where


import           Control.Applicative
import qualified Control.Arrow as Arr

import           Text.XML.PolySoup.Predicate


-- | An XML forest parser.
newtype P a b = P { runP :: [a] -> Maybe (b, [a]) }

instance Functor (P a) where
    fmap f (P p) = P $ fmap (fmap $ Arr.first f) p

instance Applicative (P a) where
    pure x = P $ Just . (x,)
    P p <*> P q = P $ \t0 -> do
        (f, t1) <- p t0
        (x, t2) <- q t1  
        return (f x, t2)

instance Alternative (P a) where
    empty = P $ \_ -> Nothing
    P p <|> P q = P $ \t -> p t <|> q t

instance Monad (P a) where
    return = pure
    P p >>= f = P $ \t0 -> do
        (x, t1) <- p t0
        runP (f x) t1


-- | Evaluate parser on the given XML forest.
evalP :: P a b -> [a] -> Maybe b
evalP p = fmap fst . runP p


---------------------------------------------------------------------
-- Sequential parsers
---------------------------------------------------------------------


-- | Make parser from a predicate.
one :: Q a b -> P a b
one (Q p) = P $ \tts -> case tts of
    (t:ts)  -> (,ts) <$> p t
    []      -> Nothing


---------------------------------------------------------------------
-- Selective parsers
---------------------------------------------------------------------


-- | Select the first tree satisfying the given predicate.
first :: Q a b -> P a b
first (Q p) = P $ go [] where
    go acc (t:ts) = case p t of
        Just v  -> Just (v, reverse acc ++ ts)
        Nothing -> go (t:ts) ts
    go _ [] = Nothing


-- | Select every tree satisfying the given predicate.
every :: Q a b -> P a [b]
every (Q p) =
    P $ prep . foldl upd ([], [])
  where
    prep (x, y) = Just (reverse x, reverse y)
    upd (vs, acc) t = case p t of
        Just v  -> (v:vs, acc)
        Nothing -> (vs, t:acc)
