{-# LANGUAGE TupleSections #-}


-- | The module re-exports sub-modules of the library:
--
--   * "Text.XML.PolySoup.XmlTree": defines XML tree and provides XML parsing
--   and printing utilities, which can be used to transform between a tagsoup
--   representation of an XML and its tree representation.
--
--   * "Text.XML.PolySoup.Predicate": defines a generic predicate type, which
--   is subsequently used to implement a simple XML tag parser and an XML
--   tree parser.
--
--   * "Text.XML.PolySoup.Parser": defines a generic parser which can be used,
--   in particular, to parse XML forests.  The main characteristic of the parser
--   is that it can be used in a sequential (sub-trees are processed in order)
--   and a selective (subtrees are process regardless of their position) way.
--
--   * "Text.XML.PolySoup.Tag": provides basic tag-level predicates.
--
--   * "Text.XML.PolySoup.Combine": provides many parsing combinators.  
--   In particular, it provides a family of combinators which can be used
--   for simple traversals of an XML tree.  Different kinds of combinators
--   can be interleaved to construct a composite parser.


module Text.XML.PolySoup
( module Text.XML.PolySoup.XmlTree
, module Text.XML.PolySoup.Predicate
, module Text.XML.PolySoup.Parser
, module Text.XML.PolySoup.Tag
, module Text.XML.PolySoup.Combine
) where


import            Text.XML.PolySoup.XmlTree
import            Text.XML.PolySoup.Predicate
import            Text.XML.PolySoup.Parser
import            Text.XML.PolySoup.Tag
import            Text.XML.PolySoup.Combine
