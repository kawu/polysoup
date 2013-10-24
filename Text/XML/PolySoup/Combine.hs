-- | The module provides some common XML tree parsing combinators.


module Text.XML.PolySoup.Combine
(
-- * XPath-like combinators
  (</>)
, (>/>)
, (/>)
, (//>)
-- * 
) where


import           Prelude hiding (any)
import           Control.Applicative
import           Data.Tree
import           Text.HTML.TagSoup (Tag)

import           Text.XML.PolySoup.XmlTree
import           Text.XML.PolySoup.Predicate


-- | Combine the tag parser with the XML parser.  The XML parser can
-- depend on the value of tag parser and will be called multiple times
-- for tag children elements.
(>/>) :: P (Tag s) a -> (a -> P (XmlTree s) b) -> P (XmlTree s) [b]
(>/>) (P p) q = P $ \(Node t ts) -> case p t of
    Nothing -> Nothing
    Just v  -> Just [w | Just w <- runP q' <$> ts] where q' = q v
infixr 2 >/>


-- | Combine the tag parser with the XML parser.  The XML parser will
-- be called multiple times for tag children elements.
(</>) :: P (Tag s) a -> P (XmlTree s) b -> P (XmlTree s) (a, [b])
(</>) (P p) q = P $ \(Node t ts) -> case p t of
    Nothing -> Nothing
    Just v  -> Just (v, [w | Just w <- runP q <$> ts])
infixr 2 </>


-- | Combine the tag parser with the XML parser.  The XML parser will
-- be called multiple times for tag children elements.
(/>) :: P (Tag s) a -> P (XmlTree s) b -> P (XmlTree s) [b]
(/>) p q = p >/> const q
infixr 2 />


-- | Similar to '/>' combinator but runs the XML parser for all
-- descendant XML elements, not only for its children.
(//>) :: P (Tag s) a -> P (XmlTree s) b -> P (XmlTree s) [b]
(//>) (P p) q = P $ \(Node t ts) -> case p t of
    Nothing -> Nothing
    Just _  -> Just $ concatMap g ts
  where
    g t = case runP q t of
        Nothing -> unJust $ runP (any //> q) t
        Just w  -> [w]
infixr 2 //>


---------------------------------------------------------------------
-- Misc
---------------------------------------------------------------------


unJust :: Maybe [a] -> [a]
unJust (Just xs) = xs
unJust Nothing   = []
