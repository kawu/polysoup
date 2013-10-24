-- | The module provides tag-level predicates.


module Text.XML.PolySoup.Tag
(
-- * Tag queries
  getName
, getText
, getAttr
, getAtts

-- * Extraction predicates
, name
, text
, attr
, atts

-- * Tag predicates
, innerTag
, leafTag
, textTag
, commentTag
, warningTag
, positionTag
, named
, hasAttr
, hasAttrVal
) where


import           Prelude hiding (any)
import           Control.Monad ((<=<))
import           Data.Maybe (isJust)
import           Text.HTML.TagSoup

import           Text.XML.PolySoup.Predicate


---------------------------------------------------------------------
-- Tag queries
---------------------------------------------------------------------


-- | Get name of the tag.
getName :: Tag s -> Maybe s
getName t = case t of
    TagOpen x _     -> Just x
    TagClose x      -> Just x
    TagText x       -> Just x
    TagComment x    -> Just x
    TagWarning x    -> Just x
    TagPosition _ _ -> Nothing


-- | Get contents of the text node.
-- A synonym for `maybeTagText`.
getText :: Tag s -> Maybe s
getText = maybeTagText


-- | Get name of the tag.
getAtts :: Tag s -> Maybe [(s, s)]
getAtts (TagOpen _ x)   = Just x
getAtts _               = Nothing


-- | Get value of the attribute.
getAttr :: Eq s => s -> Tag s -> Maybe s
getAttr x = lookup x <=< getAtts


---------------------------------------------------------------------
-- Core predicates
---------------------------------------------------------------------


-- | Internal node (i.e., an opening tag).
innerTag :: Q (Tag s) (Tag s)
innerTag = satisfy isTagOpen


-- | Leaf node (everything but an opening tag).
leafTag :: Q (Tag s) (Tag s)
leafTag = satisfy $ not . isTagOpen


-- | A text node.
textTag :: Q (Tag s) (Tag s)
textTag = satisfy isTagText


-- | A comment node.
commentTag :: Q (Tag s) (Tag s)
commentTag = satisfy isTagComment


-- | A warning node.
warningTag :: Q (Tag s) (Tag s)
warningTag = satisfy isTagWarning


-- | A position node.
positionTag :: Q (Tag s) (Tag s)
positionTag = satisfy isTagPosition


-- | Does it have a given name?
named :: Eq s => s -> Q (Tag s) (Tag s)
named x = satisfy $ justSatisfy (==x) . getName


-- | Does it have a given attribute?
hasAttr :: Eq s => s -> Q (Tag s) (Tag s)
hasAttr x = satisfy $ isJust . getAttr x


-- | Does it have a given attribute with a given value?
hasAttrVal :: Eq s => s -> s -> Q (Tag s) (Tag s)
hasAttrVal x y = satisfy $ justSatisfy (==y) . getAttr x


---------------------------------------------------------------------
-- Convenience predicates
---------------------------------------------------------------------


-- | Extract the tag name.
name :: Q (Tag s) s
name = Q getName


-- | Extract textual contents of the text node.
text :: Q (Tag s) s
text = Q getText


-- | Extract the attribute value.
attr :: Eq s => s -> Q (Tag s) s
attr = Q . getAttr


-- | Extract the attribute value.
atts :: Q (Tag s) [(s, s)]
atts = Q getAtts


---------------------------------------------------------------------
-- Misc
---------------------------------------------------------------------


-- | Is it a `Just` value and does it satisfy the given property?
justSatisfy :: (a -> Bool) -> Maybe a -> Bool
justSatisfy = maybe False


-- | Test if a tag is a comment.
isTagComment :: Tag s -> Bool
isTagComment (TagComment _) = True
isTagComment _ = False
