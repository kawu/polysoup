-- | Predicates on XML tags (tree nodes).


module Text.XML.PolySoup.Node
(
-- * Core
  node
, leaf
, text
, comment
, warning
, position
, hasName
, hasAttr
, hasAttrVal

-- * Convenience
, name
) where


import           Prelude hiding (any)
import           Control.Monad ((<=<))
import           Data.Maybe (isJust)
import           Text.HTML.TagSoup

import           Text.XML.PolySoup.Predicate


---------------------------------------------------------------------
-- Core predicates
---------------------------------------------------------------------


-- | Internal node (i.e., an opening tag).
node :: P (Tag s) (Tag s)
node = satisfy isTagOpen


-- | Leaf node (everything but an opening tag).
leaf :: P (Tag s) (Tag s)
leaf = satisfy $ not . isTagOpen


-- | A text node.
text :: P (Tag s) (Tag s)
text = satisfy isTagText


-- | A comment node.
comment :: P (Tag s) (Tag s)
comment = satisfy isTagComment


-- | A warning node.
warning :: P (Tag s) (Tag s)
warning = satisfy isTagWarning


-- | A position node.
position :: P (Tag s) (Tag s)
position = satisfy isTagPosition


-- | Does it have a given name?
hasName :: Eq s => s -> P (Tag s) (Tag s)
hasName x = satisfy $ justSatisfy (==x) . getName


-- | Does it have a given attribute?
hasAttr :: Eq s => s -> P (Tag s) (Tag s)
hasAttr x = satisfy $ isJust . (lookup x <=< getAtts)


-- | Does it have a given attribute with a given value?
hasAttrVal :: Eq s => s -> s -> P (Tag s) (Tag s)
hasAttrVal x y = satisfy $ justSatisfy (==y) . (lookup x <=< getAtts)


---------------------------------------------------------------------
-- Convenience predicates
---------------------------------------------------------------------


-- | Extract the tag name.
name :: P (Tag s) s
name = P getName


---------------------------------------------------------------------
-- Misc
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


-- | Get name of the tag.
getAtts :: Tag s -> Maybe [(s, s)]
getAtts (TagOpen _ x) = Just x
getAtts _               = Nothing


-- | Is it a `Just` value and does it satisfy the given property?
justSatisfy :: (a -> Bool) -> Maybe a -> Bool
justSatisfy = maybe False


-- | Test if a tag is a comment.
isTagComment :: Tag s -> Bool
isTagComment (TagComment _) = True
isTagComment _ = False
