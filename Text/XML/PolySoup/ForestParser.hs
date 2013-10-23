{-# LANGUAGE TupleSections #-}


module Text.XML.PolySoup.ForestParser
(
-- * Core
  ForestParser (..)
, evalFP
-- * Sequential parsers
, tree
-- * Selective parsers
, first
, every
) where


import           Control.Applicative
import qualified Control.Arrow as Arr

import           Text.XML.PolySoup.XmlTree
import           Text.XML.PolySoup.TreeParser


-- | An XML forest parser.
newtype ForestParser s a = FP
    { runFP :: XmlForest s -> Maybe (a, XmlForest s) }

instance Functor (ForestParser s) where
    fmap f (FP p) = FP $ fmap (fmap $ Arr.first f) p

instance Applicative (ForestParser s) where
    pure x = FP $ Just . (x,)
    FP p <*> FP q = FP $ \t0 -> do
        (f, t1) <- p t0
        (x, t2) <- q t1  
        return (f x, t2)

instance Alternative (ForestParser s) where
    empty = FP $ \_ -> Nothing
    FP p <|> FP q = FP $ \t -> p t <|> q t

instance Monad (ForestParser s) where
    return = pure
    FP p >>= f = FP $ \t0 -> do
        (x, t1) <- p t0
        runFP (f x) t1


-- | Evaluate parser on the given XML forest.
evalFP :: ForestParser s a -> XmlForest s -> Maybe a
evalFP p = fmap fst . runFP p


---------------------------------------------------------------------
-- Sequential parsers
---------------------------------------------------------------------


-- | Generic predicate on a tree.
tree :: TreeParser s a -> ForestParser s a
tree (TP p) = FP $ \tts -> case tts of
    (t:ts)  -> (,ts) <$> p t
    []      -> Nothing


---------------------------------------------------------------------
-- Selective parsers
---------------------------------------------------------------------


-- | Select the first tree satisfying the given predicate.
first :: TreeParser s a -> ForestParser s a
first (TP p) = FP $ go [] where
    go acc (t:ts) = case p t of
        Just v  -> Just (v, reverse acc ++ ts)
        Nothing -> go (t:ts) ts
    go _ [] = Nothing


-- | Select every tree satisfying the given predicate.
every :: TreeParser s a -> ForestParser s [a]
every (TP p) =
    FP $ prep . foldl upd ([], [])
  where
    prep (x, y) = Just (reverse x, reverse y)
    upd (vs, acc) t = case p t of
        Just v  -> (v:vs, acc)
        Nothing -> (vs, t:acc)
