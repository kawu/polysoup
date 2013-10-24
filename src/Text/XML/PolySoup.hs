{-# LANGUAGE TupleSections #-}


-- | The module re-exports individual submodules of the library.


module Text.XML.PolySoup
( module Text.XML.PolySoup.XmlTree
-- $xmltree
, module Text.XML.PolySoup.Predicate
-- $predicate
, module Text.XML.PolySoup.Parser
-- $parser
, module Text.XML.PolySoup.Tag
-- $tag
, module Text.XML.PolySoup.Combine
-- $combine
) where


import            Text.XML.PolySoup.XmlTree
import            Text.XML.PolySoup.Predicate
import            Text.XML.PolySoup.Parser
import            Text.XML.PolySoup.Tag
import            Text.XML.PolySoup.Combine


{- $xmltree
Defines XML tree and provides XML parsing and printing utilities, which can
be used to transform between a tagsoup representation of an XML and its tree
representation. -}

{- $predicate
Defines a generic predicate type, which is subsequently used to implement a
simple XML tag parser and an XML tree parser. -}

{- $parser
Defines a generic parser which can be used, in particular, to parse XML
forests.  The main characteristic of the parser is that it can be used in
a sequential (sub-trees are processed in order) and a selective (subtrees
are process regardless of their position) way. -}

{- $tag
Provides basic tag-level predicates. -}

{- $combine
Provides many parsing combinators.  In particular, it provides a family
of combinators which can be used for simple traversals of an XML tree.
Different kinds of combinators can be interleaved to construct a
composite parser. -}
