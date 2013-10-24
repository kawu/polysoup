{-# LANGUAGE TupleSections #-}


-- | The module provides some common XML tree parsing combinators.
-- There are two main groups of combinators: XPath-like combinators
-- and tag/forest combinators.  Use combinators from the first group
-- if possible, since they are generally easier too use and generate
-- results in a lazy manner.
--
-- The second class contains more powerful combinators which can be used
-- to parse the contents of an XML node in a generic way.  Note, that
-- combinators from the two groups can be interleaved -- you can use
-- forest parser to construct a tree predicate, but you can also use
-- a tree predicate as an elementary forest parser (see the
-- Text.XML.PolySoup.FParser module).


module Text.XML.PolySoup.Combine
(
-- * XPath-like combinators
  (</>)
, (>/>)
, (/>)
, (//>)
-- * Tag/forest parsing combinators
, join
, joinP
, joinL
, joinR
, (>^>)
, (<^>)
, (^>)
, (<^)
) where


import           Prelude hiding (any)
import           Control.Applicative
import           Data.Tree
import           Text.HTML.TagSoup (Tag)

import           Text.XML.PolySoup.XmlTree
import           Text.XML.PolySoup.Predicate
import           Text.XML.PolySoup.FParser


---------------------------------------------------------------------
-- XPath
---------------------------------------------------------------------


-- TODO: usun to ciagle rozroznienie na Nothing i Just.  Monada?


-- | Combine a tag predicate with an XML predicate.  The XML predicate can
-- depend on the value of tag parser and will be called multiple times for
-- tag children elements.
(>/>) :: P (Tag s) a -> (a -> P (XmlTree s) b) -> P (XmlTree s) [b]
(>/>) (P p) q = P $ \(Node t ts) -> case p t of
    Just v  -> Just [w | Just w <- runP q' <$> ts] where q' = q v
    Nothing -> Nothing
infixr 2 >/>


-- | Combine the tag parser with the XML parser.  The XML parser will
-- be called multiple times for tag children elements.
(</>) :: P (Tag s) a -> P (XmlTree s) b -> P (XmlTree s) (a, [b])
(</>) (P p) q = P $ \(Node t ts) -> case p t of
    Just v  -> Just (v, [w | Just w <- runP q <$> ts])
    Nothing -> Nothing
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
    Just _  -> Just $ concatMap g ts
    Nothing -> Nothing
  where
    g t = case runP q t of
        Nothing -> unJust $ runP (any //> q) t
        Just w  -> [w]
infixr 2 //>


---------------------------------------------------------------------
-- Tree predicate `combine` forest parser
---------------------------------------------------------------------


-- | Combine the tag predicate with the forest parser which will be used
-- to parse contents of the tag element.
join :: P (Tag s) a -> (a -> FParser s b) -> P (XmlTree s) b
join (P p) q = P $ \(Node t ts) -> flip evalFP ts . q =<< p t


-- | Combine the tag predicate with the forest parser which will be used
-- to parse contents of the tag element.
joinP :: P (Tag s) a -> FParser s b -> P (XmlTree s) (a, b)
joinP p q = join p $ \x -> (x,) <$> q


-- | Combine the tag predicate with the orest parser which will be used
-- to parse contents of the tag element.  Only results of the forest parser
-- will be returned.
joinR :: P (Tag s) a -> FParser s b -> P (XmlTree s) b
joinR p q = snd <$> joinP p q

-- | Combine the tag predicate with the orest parser which will be used
-- to parse contents of the tag element.  Only results of the tag predicate
-- will be returned (the contents have to be successfully parsed, though).
joinL :: P (Tag s) a -> FParser s b -> P (XmlTree s) a
joinL p q = fst <$> joinP p q


-- | Infix version of the join combinators.
(>^>) :: P (Tag s) a -> (a -> FParser s b) -> P (XmlTree s) b
(>^>) = join
infixr 2 >^>

-- | Infix version of the joinP combinators.
(<^>) :: P (Tag s) a -> FParser s b -> P (XmlTree s) (a, b)
(<^>) = joinP
infixr 2 <^>

-- | Infix version of the joinR combinators.
(^>) :: P (Tag s) a -> FParser s b -> P (XmlTree s) b
(^>) = joinR
infixr 2 ^>

-- | Infix version of the joinL combinators.
(<^) :: P (Tag s) a -> FParser s b -> P (XmlTree s) a
(<^) = joinL
infixr 2 <^




---------------------------------------------------------------------
-- Misc
---------------------------------------------------------------------


unJust :: Maybe [a] -> [a]
unJust (Just xs) = xs
unJust Nothing   = []
