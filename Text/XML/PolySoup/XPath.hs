module Text.XML.PolySoup.XPath
( (</>)
, (>/>)
, (/>)
, (//>)
) where


import           Prelude hiding (any)
import           Control.Applicative
import           Data.Tree

import           Text.XML.PolySoup.TagPred
import           Text.XML.PolySoup.TreeParser


-- | Combine the tag parser with the XML parser.  The XML parser can
-- depend on the value of tag parser and will be called multiple times
-- for tag children elements.
(>/>) :: TagPred s a -> (a -> TreeParser s b) -> TreeParser s [b]
(>/>) (TagPred p) q = TP $ \(Node t ts) -> case p t of
    Nothing -> Nothing
    Just v  -> Just [w | Just w <- runTP q' <$> ts] where q' = q v
infixr 2 >/>


-- | Combine the tag parser with the XML parser.  The XML parser will
-- be called multiple times for tag children elements.
(</>) :: TagPred s a -> TreeParser s b -> TreeParser s (a, [b])
(</>) (TagPred p) q = TP $ \(Node t ts) -> case p t of
    Nothing -> Nothing
    Just v  -> Just (v, [w | Just w <- runTP q <$> ts])
infixr 2 </>


-- | Combine the tag parser with the XML parser.  The XML parser will
-- be called multiple times for tag children elements.
(/>) :: TagPred s a -> TreeParser s b -> TreeParser s [b]
(/>) p q = snd <$> (p </> q)
infixr 2 />


-- | Similar to '/>' combinator but runs the XML parser for all
-- descendant XML elements, not only for its children.
(//>) :: TagPred s a -> TreeParser s b -> TreeParser s [b]
(//>) (TagPred p) q = TP $ \(Node t ts) -> case p t of
    Nothing -> Nothing
    Just _  -> Just $ concatMap g ts
  where
    g t = case runTP q t of
        Nothing -> unJust $ runTP (any //> q) t
        Just w  -> [w]
infixr 2 //>


---------------------------------------------------------------------
-- Misc
---------------------------------------------------------------------


unJust :: Maybe [a] -> [a]
unJust (Just xs) = xs
unJust Nothing   = []
