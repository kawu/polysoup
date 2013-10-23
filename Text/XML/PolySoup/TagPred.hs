module Text.XML.PolySoup.TagPred
(
-- * Types
  TagPred (..)
-- * Core predicates
, any
, satisfy
, node
, leaf
, text
, comment
, warning
, position
, hasName
, hasAttr
, hasAttrVal
-- * Convenience predicates
, name
) where


import           Prelude hiding (any)
import           Control.Applicative
import           Control.Monad ((<=<))
import           Data.Maybe (isJust)
import qualified Text.HTML.TagSoup as S


-- | A tag predicate checks if the HTML tag satisfies some properties
-- and extracts attribute values.  You can compose tag predicates using
-- Functor, Applicative and Alternative operators: '*>', '<*', '<|>' etc.
-- TODO: Change name to TagPredicate or TagParser?
--
-- Another point: it should be named neither parser nor a predicate, but
-- rather something like extractor?
--
-- Yet another: it doesn't make sense to use `many` or `some` on the
-- tag predicate.  Point it out!
newtype TagPred s a = TagPred (S.Tag s -> Maybe a)

instance Functor (TagPred s) where  
    fmap f (TagPred g) = TagPred $ fmap (fmap f) g

instance Applicative (TagPred s) where  
    pure = TagPred . const . Just
    TagPred f <*> TagPred p = TagPred $ \t -> f t <*> p t

instance Alternative (TagPred s) where  
    empty = TagPred $ \_ -> Nothing
    TagPred p <|> TagPred p' = TagPred $ \t -> p t <|> p' t


---------------------------------------------------------------------
-- Core tag predicates
---------------------------------------------------------------------


-- | Predicate which is always satisfied.
any :: TagPred s (S.Tag s)
any = TagPred Just


-- | Check if the given predicate is satisfied.
satisfy :: (S.Tag s -> Bool) -> TagPred s (S.Tag s)
satisfy p = TagPred $ \t -> if p t
    then Just t
    else Nothing


-- | Internal node (i.e., an opening tag).
node :: TagPred s (S.Tag s)
node = satisfy S.isTagOpen


-- | Leaf node (everything but an opening tag).
leaf :: TagPred s (S.Tag s)
leaf = satisfy $ not . S.isTagOpen


-- | A text node.
text :: TagPred s (S.Tag s)
text = satisfy S.isTagText


-- | A comment node.
comment :: TagPred s (S.Tag s)
comment = satisfy isTagComment


-- | A warning node.
warning :: TagPred s (S.Tag s)
warning = satisfy S.isTagWarning


-- | A position node.
position :: TagPred s (S.Tag s)
position = satisfy S.isTagPosition


-- | Does it have a given name?
hasName :: Eq s => s -> TagPred s (S.Tag s)
hasName x = satisfy $ justSatisfy (==x) . getName


-- | Does it have a given attribute?
hasAttr :: Eq s => s -> TagPred s (S.Tag s)
hasAttr x = satisfy $ isJust . (lookup x <=< getAtts)


-- | Does it have a given attribute with a given value?
hasAttrVal :: Eq s => s -> s -> TagPred s (S.Tag s)
hasAttrVal x y = satisfy $ justSatisfy (==y) . (lookup x <=< getAtts)


---------------------------------------------------------------------
-- Convenience tag predicates
---------------------------------------------------------------------


-- | Extract the tag name.
name :: TagPred s s
name = TagPred getName


---------------------------------------------------------------------
-- Misc
---------------------------------------------------------------------


-- | Get name of the tag.
getName :: S.Tag s -> Maybe s
getName t = case t of
    S.TagOpen x _       -> Just x
    S.TagClose x        -> Just x
    S.TagText x         -> Just x
    S.TagComment x      -> Just x
    S.TagWarning x      -> Just x
    S.TagPosition _ _   -> Nothing


-- | Get name of the tag.
getAtts :: S.Tag s -> Maybe [(s, s)]
getAtts (S.TagOpen _ x) = Just x
getAtts _               = Nothing


-- | Is it a `Just` value and does it satisfy the given property?
justSatisfy :: (a -> Bool) -> Maybe a -> Bool
justSatisfy = maybe False


-- | Test if a tag is a comment.
isTagComment :: S.Tag s -> Bool
isTagComment (S.TagComment _) = True
isTagComment _ = False
