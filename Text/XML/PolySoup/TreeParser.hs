module Text.XML.PolySoup.TreeParser
( TreeParser (..)
) where


import           Control.Applicative

import           Text.XML.PolySoup.XmlTree


-- | An XML tree parser.
-- We use the `Nothing` value to represent the parse failure.
-- TODO: Note the similarity between TagParser, TreeParser and
-- ForestParser types.  They have almost the same instances.
-- Perhaps we can represent them using one genering parser type?
-- TODO: It's not really a parser, but rather an extractor or
-- something like that.  Point it out?
newtype TreeParser s a = TP { runTP :: XmlTree s -> Maybe a }

instance Functor (TreeParser s) where
    fmap f (TP p) = TP $ fmap (fmap f) p

instance Applicative (TreeParser s) where  
    pure = TP . const . Just
    TP p <*> TP q = TP $ \t -> p t <*> q t

instance Alternative (TreeParser s) where  
    empty = TP $ \_ -> Nothing
    TP p <|> TP q = TP $ \t -> p t <|> q t
