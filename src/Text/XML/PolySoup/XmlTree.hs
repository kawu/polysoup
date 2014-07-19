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
, XmlForest
-- ** Parsing
, parseTree
, parseForest
-- ** Rendering
, renderTree
, renderForest
) where


import           Control.DeepSeq (NFData, deepseq)
import           Data.Tree
import           Text.StringLike
import qualified Text.HTML.TagSoup as S
import           Text.ParserCombinators.Poly.Lazy


---------------------------------------------------------------------
-- Types
---------------------------------------------------------------------


-- | A lazy XML parser.
type XmlParser s a = Parser (S.Tag s) a


-- | A parsed XML tree.  Closing tags are not preserved.
type XmlTree s = Tree (S.Tag s)

-- data XmlTree s = Node
--     { rootLabel :: S.Tag s
--     , subForest :: Forest a }
--     deriving (Show, Eq, Ord)


-- | A parsed XML forest.  Closing tags are not preserved.
type XmlForest s = [XmlTree s]


---------------------------------------------------------------------
-- Parsing XML
---------------------------------------------------------------------


-- | Parse XML tree from a list of tags.
parseTree :: (NFData s, StringLike s) => [S.Tag s] -> XmlTree s
parseTree = fst . runParser xmlTreeP


-- | Parse XML forest from a list of tags.  Note, that if the XML file
-- has additional headers, the `parseForest` function has to be used to
-- parse it correctly.
parseForest :: (NFData s, StringLike s) => [S.Tag s] -> XmlForest s
parseForest = fst . runParser (many xmlTreeP)


-- | A parser from tags to an XML tree.
xmlTreeP :: (NFData s, StringLike s) => XmlParser s (XmlTree s)
xmlTreeP = leafP <|> nodeP
-- xmlTreeP = nodeP <|> leafP


-- | Internal node parser.
nodeP :: (NFData s, StringLike s) => XmlParser s (XmlTree s)
nodeP = do
    x <- satisfy S.isTagOpen
    x `tagOpenSeq` Node x <$> many xmlTreeP
        <* satisfy (S.isTagCloseName $ tagName x)
  where
    tagName (S.TagOpen x _) = x
    tagName _ = error "tagName: not an open tag"
    -- Without `deepseq` even the simple `renderForest . parseForest` leads
    -- to a memory leak.  Tested multiple times w.r.t. tagsoup-0.13.1.
    tagOpenSeq (S.TagOpen x xs) = deepseq (x, xs)
    tagOpenSeq _ = id


-- | Leaf node parser.
leafP :: (NFData s, StringLike s) => XmlParser s (XmlTree s)
leafP = fmap
    (flip Node [])
--     (satisfy $ \x -> not (S.isTagOpen x || S.isTagClose x))
    (satisfy $ \x -> not (isTagOpen x || S.isTagClose x))
  where
    isTagOpen (S.TagOpen xs _) = case uncons xs of
        -- Headers (e.g. <?xml ...> and <!CDATA ...>) do not have
        -- corresponding closing tags, therefore have to be treated
        -- as leaves.
        Just (x, _) -> x /= '!' && x /= '?'
        Nothing     -> True
    isTagOpen _ = False
    

---------------------------------------------------------------------
-- Rendering XML
---------------------------------------------------------------------


-- | Render XML tree tags.
renderTree :: XmlTree s -> [S.Tag s]
renderTree (Node v xs) = if S.isTagOpen v
    then v : renderForest xs ++ [endFrom v]
    else [v]


-- | Render XML forest tags.
renderForest :: XmlForest s -> [S.Tag s]
renderForest = concatMap renderTree


-- | Make closing tag from the opening tag.
endFrom :: S.Tag s -> S.Tag s
endFrom (S.TagOpen x _) = S.TagClose x
endFrom _               = error "endFrom: not an opening tag"
