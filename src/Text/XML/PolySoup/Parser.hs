{-# LANGUAGE TupleSections #-}


-- | The module defines a generic parser which can be used, in particular,
-- to parse XML forests.  The main characteristic of the parser is that it
-- can be used in a sequential (sub-trees are processed in order) and a
-- selective (subtrees are process regardless of their position) way.


module Text.XML.PolySoup.Parser
(
-- * Core
  P (..)
, evalP

-- * Parsing
-- ** Selective
-- , find
, first
, every
, every'
-- ** Sequential
, pop
-- ** Peek
, peek
, spy
-- ** Utilities
, many_
) where


import           Control.Applicative
import qualified Control.Arrow as Arr
import           Data.Maybe (catMaybes)

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
-- Selective parsers
---------------------------------------------------------------------


-- -- | A synonym to `first`.
-- find :: Q a b -> P a b
-- find = first


-- | Find the first tree satisfying the given predicate.
first :: Q a b -> P a b
first (Q p) = P $ go [] where
    go acc (t:ts) = case p t of
        Just v  -> Just (v, reverse acc ++ ts)
        Nothing -> go (t:acc) ts
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


-- | A lazy version of `every` which "forgets" non-matching subtrees
-- along the way.
every' :: Q a b -> P a [b]
every' (Q p) =
    let prep xs = Just (xs, [])
    in  P $ prep . catMaybes . map p


---------------------------------------------------------------------
-- Sequential parsers
---------------------------------------------------------------------


-- | Check, if the first tree satisfies the given predicate.
pop :: Q a b -> P a b
pop (Q p) = P $ \tts -> case tts of
    (t:ts)  -> (,ts) <$> p t
    []      -> Nothing


---------------------------------------------------------------------
-- Peek
---------------------------------------------------------------------


-- | Like `pop`, but doesn't consume the tree.
peek :: Q a b -> P a b
peek (Q p) = P $ \tts -> case tts of
    (t:_)   -> (,tts) <$> p t
    []      -> Nothing


-- | Like `first`, but doesn't consume the tree.
spy :: Q a b -> P a b
spy (Q p) = P $ \tts ->
    let go (t:ts) = case p t of
            Just v  -> Just (v, tts)
            Nothing -> go ts
        go [] = Nothing
    in  go tts


---------------------------------------------------------------------
-- Utilities
---------------------------------------------------------------------


-- | Many combinator which ignores parsing results.
many_ :: Alternative f => f a -> f ()
many_ v = many_v
  where
    many_v = some_v <|> pure ()
    some_v = v *> many_v
