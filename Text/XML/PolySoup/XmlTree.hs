-- | XML as a tree of XML tags. 
--
-- The module provides an `XmlTree` data type, which can be used to represent
-- a parsed XML file.  The `XmlTree` structure can be generated lazily by using
-- the `parseTree` (or `parseForest`) function on any string-like input
-- supported by the tagsoup library.
--
-- Note, that the parsing functions do not validate correctness of the input
-- XML data.


module Text.XML.PolySoup.XmlTree
(
-- * XML Tree
  XmlTree
-- ** Parsing
, parseTree
, parseForest
-- ** Rendering
, renderTree
, renderForest
) where


import           Data.Tree
import qualified Text.HTML.TagSoup as S
import           Text.ParserCombinators.Poly.Lazy


---------------------------------------------------------------------
-- Types
---------------------------------------------------------------------


-- | A lazy XML parser.
type XmlParser s a = Parser (S.Tag s) a


-- | A parsed XML tree.  In nodes the content/opening tags are preserved.
type XmlTree s = Tree (S.Tag s)


---------------------------------------------------------------------
-- Parsing XML
---------------------------------------------------------------------


-- | Parse XML tree from a list of tags.
parseTree :: Eq s => [S.Tag s] -> XmlTree s
parseTree = fst . runParser xmlTreeP


-- | Parse XML forest from a list of tags.  Note, that if the XML file
-- has additional headers, the `parseForest` function has to be used to
-- parse it correctly.
parseForest :: Eq s => [S.Tag s] -> [XmlTree s]
parseForest = fst . runParser (many xmlTreeP)


-- | A parser from tags to an XML tree.
xmlTreeP :: Eq s => XmlParser s (XmlTree s)
xmlTreeP = nodeP <|> leafP


-- | Internal node parser.
nodeP :: Eq s => XmlParser s (XmlTree s)
nodeP = do
    x <- satisfy S.isTagOpen
    x `seq` Node x <$> many xmlTreeP
        <* satisfy (S.isTagCloseName $ tagName x)
  where
    tagName (S.TagOpen x _) = x
    tagName _ = error "tagName: not an open tag"


-- | Leaf node parser.
leafP :: XmlParser s (XmlTree s)
leafP = fmap
    (flip Node [])
    (satisfy $ \x ->
        not (S.isTagOpen x || S.isTagClose x))
    

---------------------------------------------------------------------
-- Rendering XML
---------------------------------------------------------------------


-- | Render XML tree tags.
renderTree :: XmlTree s -> [S.Tag s]
renderTree (Node v xs) = if S.isTagOpen v
    then v : renderForest xs ++ [endFrom v]
    else [v]


-- | Render XML forest tags.
renderForest :: [XmlTree s] -> [S.Tag s]
renderForest = concatMap renderTree


-- | Make closing tag from the opening tag.
endFrom :: S.Tag s -> S.Tag s
endFrom (S.TagOpen x _) = S.TagClose x
endFrom _               = error "endFrom: not an opening tag"
