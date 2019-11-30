{-# LANGUAGE TupleSections #-}

module Text.XML.PolySoup
(
 -- * Types
  XmlParser
, TagParser
, TagPred

-- * Tag predicates
, satisfyPred
, true
, getTag
, isTagOpen
, isTagOpenName
, isTagClose
, isTagCloseName
, isTagText
, isTagComment
, tagOpenName
, tagText
, tag
, hasAttr
, getAttr
, maybeAttr

-- * XML parsing combinators
, ignore
, ignoreAny
, ignoreText
, ignoreTag
, ignoreAnyM
, cut
, findAll
, findIgnore
, findFirst
, text
, join
, joinP
, joinR
, joinL
, (>^>)
, (<^>)
, (^>)
, (<^)

-- * XPath-like combinators
, (>/>)
, (</>)
, (/>)
, (</)
, (//>)
, (<#>)
, (#>)
, (##>)

-- * Parsing
, parseTags
, tagsParseXml
, parseXml
, elemTags
, collTags

-- * Utilities
, many_
, escapeXml

, module Text.ParserCombinators.Poly.Lazy
) where

import Data.Monoid
import Control.Applicative
import Data.Char (isSpace)
import Control.Monad (guard)
import Data.Maybe (catMaybes, isJust, fromJust)
import qualified Text.HTML.TagSoup as Tag
import Text.StringLike
import Text.ParserCombinators.Poly.Lazy

-- | A tag predicate checks if the tag (HTML element) satisfies some
-- properties and extracts attribute values.  You can compose tag predicates
-- using Applicative and Alternative operators: '*>', '<*', '<|>' etc.
newtype TagPred s a = TagPred (Tag.Tag s -> Maybe a)

instance Functor (TagPred s) where  
    fmap f (TagPred g) = TagPred $ fmap (fmap f) g

instance Applicative (TagPred s) where  
    pure = TagPred . const . Just
    TagPred f <*> TagPred p = TagPred $ \t -> f t <*> p t

instance Alternative (TagPred s) where  
    empty = TagPred $ \_ -> Nothing
    TagPred p <|> TagPred p' = TagPred $ \t -> p t <|> p' t

-- | True predicate which returns the tag itself. 
getTag :: TagPred s (Tag.Tag s)
getTag = TagPred Just

fromBool :: Bool -> Maybe ()
fromBool True  = Just ()
fromBool False = Nothing

-- | Predicate which is always satisfied.
true :: TagPred s ()
true = pure ()

-- | Check if the HTML element is an open tag.
isTagOpen :: TagPred s ()
isTagOpen = TagPred (fromBool . Tag.isTagOpen)

-- | Check if the HTML element is a closing tag.
isTagClose :: TagPred s ()
isTagClose = TagPred (fromBool . Tag.isTagClose)

-- | Check if the tag is an open tag and matches the given name.
isTagOpenName :: Eq s => s -> TagPred s ()
isTagOpenName nm = TagPred (fromBool . Tag.isTagOpenName nm)

-- | Check if the tag is a closing tag and matches the given name.
isTagCloseName :: Eq s => s -> TagPred s ()
isTagCloseName nm = TagPred (fromBool . Tag.isTagCloseName nm)

-- | A shorthand for isTagOpenName.
tag :: Eq s => s -> TagPred s ()
tag = isTagOpenName

-- | Get name of the open tag.
tagOpenName :: TagPred s s
tagOpenName =
    isTagOpen *> TagPred getIt
  where
    getIt (Tag.TagOpen name _) = Just name
    getIt _ = Nothing

-- | Test if the tag is a text node.
isTagText :: TagPred s ()
isTagText = TagPred (fromBool . Tag.isTagText)

-- | Test if the tag is a text node.
isTagComment :: TagPred s ()
isTagComment =
    let isComm (Tag.TagComment {}) = True; isComm _ = False
    in  TagPred (fromBool . isComm)

-- | Get text content of the tag.
tagText :: TagPred s s
tagText = TagPred Tag.maybeTagText 

-- | Get attribute value from the open tag or Nothing if
-- the attribute is not present.  It is an alternative
-- for Tag.fromAttrib.
fromAttrib :: (Show str, Eq str, StringLike str)
           => str -> Tag.Tag str -> Maybe str
fromAttrib att (Tag.TagOpen _ atts) = lookup att atts
fromAttrib _ x = error ("(" ++ show x ++ ") is not a TagOpen")

-- | Check if the tag has the given attribute with the given value.
hasAttr :: (Show s, Eq s, StringLike s) => s -> s -> TagPred s ()
hasAttr name x =
    isTagOpen *> TagPred checkIt 
  where
    checkIt t = do
        y <- fromAttrib name t
        guard (x == y)

-- | Get attribute value from the open tag.
getAttr :: (Show s, Eq s, StringLike s) => s -> TagPred s s
getAttr name = isTagOpen *> TagPred (fromAttrib name)

-- | Get attribute value from the open tag or Nothing, if the
-- attribute is not present.
maybeAttr :: (Show s, Eq s, StringLike s) => s -> TagPred s (Maybe s)
maybeAttr name = isTagOpen *> TagPred (Just . fromAttrib name)

-- TODO: distinguish XmlParser
-- and TagParser types using newtype?

-- | XML forest parser with result type a.  
type XmlParser s a = Parser (Tag.Tag s) a
type TagParser s a = Parser (Tag.Tag s) a

-- | Many combinator which ignores parsing results.
many_ :: Alternative f => f a -> f ()
many_ v = many_v
  where
    many_v = some_v <|> pure ()
    some_v = v *> many_v

-- | Make a tag parser from the tag predicate.
satisfyPred :: TagPred s a -> TagParser s a
satisfyPred (TagPred t) =
    let q = isJust . t
    in  fromJust . t <$> satisfy q

-- | Ignore any number of XML elements on the current level.
ignore :: Eq s => XmlParser s ()
ignore = many_ ignoreAny

-- | Ignore XML tree or text element. 
ignoreAny :: Eq s => XmlParser s ()
ignoreAny = ignoreText <|> ignoreTag

-- | Ignore text element. 
ignoreText :: XmlParser s ()
ignoreText = satisfyPred isTagText

-- | Ignore XML tree. 
ignoreTag :: Eq s => XmlParser s ()
ignoreTag = do
    name <- satisfyPred tagOpenName
    name `seq` many_ ignoreAny *> satisfyPred (isTagCloseName name)

-- | Version of the ignoreAny function with a monoid result type.
ignoreAnyM :: (Eq s, Monoid m) => XmlParser s m
ignoreAnyM = const mempty <$> ignoreAny

-- | Parse text element and retrieve its content.
text :: Eq s => XmlParser s s
text = satisfyPred tagText

-- | Parse XML element using the given tag predicate and ignore
-- contents of the element.
cut :: Eq s => TagPred s a -> XmlParser s a
cut p = p </ ignoreAny

-- | Parse a list of XML elements and collect all values
-- retrieved with a given parser.
findAll :: Eq s => XmlParser s a -> XmlParser s [a]
findAll q =
    let q' = Just <$> q <|> Nothing <$ ignoreAny
    in  catMaybes <$> many q'

-- | Find fist XML element accepted by the given parser.
-- TODO: Change type to XmlParser s (Maybe a)?
findFirst :: Eq s => XmlParser s a -> XmlParser s a
findFirst q = q <|> ignoreAny *> findFirst q

-- | Find first XML element accepted be the given parser and
-- ignore the rest of elements in the collection.
findIgnore :: Eq s => XmlParser s a -> XmlParser s (Maybe a)
findIgnore q = findAll q >>= \xs -> return $ case xs of
    (x:_) -> Just x
    []    -> Nothing

-- | Combine the tag parser with the XML parser which will be used
-- to parse contents of the tag element.
join :: Eq s => TagPred s a -> (a -> XmlParser s b) -> XmlParser s b
join p q = do
    (x, name) <- satisfyPred ((,) <$> p <*> tagOpenName)
    name `seq` x `seq` q x <* satisfyPred (isTagCloseName name)

-- | Combine the tag parser with the XML parser which will be used
-- to parse contents of the tag element.  Parsing results will be
-- returned in a form of a pair.
joinP :: Eq s => TagPred s a -> XmlParser s b -> XmlParser s (a, b)
joinP p q = join p $ \x -> (x,) <$> q

-- | Combine the tag parser with the XML parser which will be used
-- to parse contents of the tag element.  Only results of the
-- XML parser will be returned.
joinR :: Eq s => TagPred s a -> XmlParser s b -> XmlParser s b
joinR p q = snd <$> joinP p q

-- | Combine the tag parser with the XML parser which will be used
-- to parse contents of the tag element.  Only results of the
-- tag parser will be returned.
joinL :: Eq s => TagPred s a -> XmlParser s b -> XmlParser s a
joinL p q = fst <$> joinP p q

-- | Infix version of the join combinators.
(>^>) :: Eq s => TagPred s a -> (a -> XmlParser s b) -> XmlParser s b
(>^>) = join
infixr 2 >^>

-- | Infix version of the joinP combinators.
(<^>) :: Eq s => TagPred s a -> XmlParser s b -> XmlParser s (a, b)
(<^>) = joinP
infixr 2 <^>

-- | Infix version of the joinR combinators.
(^>) :: Eq s => TagPred s a -> XmlParser s b -> XmlParser s b
(^>) = joinR
infixr 2 ^>

-- | Infix version of the joinL combinators.
(<^) :: Eq s => TagPred s a -> XmlParser s b -> XmlParser s a
(<^) = joinL
infixr 2 <^


-- | Combine the tag parser with the XML parser.  The XML parser will
-- be called multiple times for tag children elements.
(</>) :: Eq s => TagPred s a -> XmlParser s b -> XmlParser s (a, [b])
(</>) p q =
    joinP p (catMaybes <$> many qMaybe)
  where
    qMaybe = Just <$> q
         <|> const Nothing <$> ignoreAny
infixr 2 </>

-- | Combine the tag parser with the XML parser.  The XML parser can depend
-- on the value of tag parser and will be called multiple times for tag children
-- elements.
(>/>) :: Eq s => TagPred s a -> (a -> XmlParser s b) -> XmlParser s [b]
(>/>) p q =
    p `join` \x -> (catMaybes <$> many (qMaybe x))
  where
    qMaybe x =  Just <$> q x
            <|> const Nothing <$> ignoreAny
infixr 2 >/>

-- | Combine the tag parser with the XML parser.  The XML parser will
-- be called multiple times for tag children elements.  Only results
-- of XML parsing will be returned.
(/>) :: Eq s => TagPred s a -> XmlParser s b -> XmlParser s [b]
(/>) p q = snd <$> (p </> q) -- joinR p (many $ q <|> ignoreAnyM)
infixr 2 />

-- | Combine the tag parser with the XML parser.  The XML parser will
-- be called multiple times for tag children elements.  Only results
-- of the tag parser will be returned.
(</) :: Eq s => TagPred s a -> XmlParser s b -> XmlParser s a
(</) p q = fst <$> (p </> q) -- joinL p (many_ $ q <|> ignoreAnyM)
infixr 2 </

-- | Similar to '/>' combinator but runs the XML parser for all
-- descendant XML elements, not only for its children. 
(//>) :: Eq s => TagPred s a -> TagParser s b -> TagParser s [b]
(//>) p q =
    concat <$> joinR p (many qList)
  where
    qList = pure <$> q
        <|> (true //> q)
        <|> ignoreAnyM
infixr 2 //>

-- | Combinators with results concatenation.

-- | Similar to '</>' combinator but additionaly concatenates XML
-- parser results.
(<#>) :: (Eq s, Monoid m) => TagPred s a -> XmlParser s m -> XmlParser s (a, m)
(<#>) p q =
    let mc (x, xs) = (x, mconcat xs)
    in  mc <$> (p </> q)
infixr 2 <#>

-- | Similar to '/>' combinator but additionaly concatenates XML
-- parser results.
(#>) :: (Eq s, Monoid m) => TagPred s a -> XmlParser s m -> XmlParser s m
(#>) p q = mconcat <$> (p /> q)
infixr 2 #>

-- | Similar to '//>' combinator but additionaly concatenates XML
-- parser results.
(##>) :: (Eq s, Monoid m) => TagPred s a -> TagParser s m -> TagParser s m
(##>) p q = mconcat <$> (p //> q)
infixr 2 ##>

relevant :: StringLike s => Tag.Tag s -> Bool
relevant (Tag.TagOpen name _)
    | name == fromString "?xml" = False
    | otherwise = True
relevant (Tag.TagClose _) = True
relevant (Tag.TagText s) = not $ null $ trim $ toString s
relevant _ = False

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

-- | Parser the given string to the list of tags.
parseTags :: StringLike s => s -> [Tag.Tag s]
parseTags = filter relevant . Tag.parseTags

-- | Parser the given tag list with the given XML parser.
tagsParseXml :: StringLike s => XmlParser s b -> [Tag.Tag s] -> b
tagsParseXml p = fst . runParser p

-- | Parser the given string with the given XML parser.
parseXml :: StringLike s => XmlParser s b -> s -> b
parseXml p = tagsParseXml p . parseTags

-- | Collect all tags of the parsed XML element.
elemTags :: Eq s => XmlParser s [Tag.Tag s]
elemTags = trueElemTags <|> (:[]) <$> textTag

trueElemTags :: Eq s => XmlParser s [Tag.Tag s]
trueElemTags = do
    (beg, name) <- satisfyPred ((,) <$> getTag <*> tagOpenName)
    inside <- beg `seq` name `seq` collTags
    end <- satisfyPred (getTag <* isTagCloseName name)
    return (beg : inside ++ [end])

-- | Return the underlying text element.
textTag :: XmlParser s (Tag.Tag s)
textTag = fst <$> satisfyPred ((,) <$> getTag <*> isTagText)

-- | Retrieve tags related to a collection of XML elements.
collTags :: Eq s => XmlParser s [Tag.Tag s]
collTags = concat <$> many elemTags

-- | Escape XML string.
escapeXml :: StringLike str => str -> str
escapeXml = Tag.escapeHTML
