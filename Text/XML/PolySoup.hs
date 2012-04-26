{-# LANGUAGE TupleSections #-}

module Text.XML.PolySoup
( XmlParser
, TagPred
, emptyPred
, isTagOpen
, isTagClose
, isTagOpenName
, tag
, hasAttr
, getAttr
, many_
, satisfyPred
, ignoreAny
, ignoreAnyM
, ignoreText
, ignoreTextM
, ignoreTag
, ignoreTagM
, join
, joinR
, joinL
, (/>)
, (//>)
, (/@)
, (//@)
, (/!)
, (//!)
, mkGenP
, mkListP
, mkFirstP
, mkLastP
, mkP
, parseXML
, module Text.ParserCombinators.Poly.Lazy
) where

import System.Environment (getArgs)
import Data.Monoid
import Control.Applicative
import Data.Char (isSpace)
import Control.Monad (forM_)
import Data.Maybe (isJust, fromJust)
import qualified Text.HTML.TagSoup as Tag
import Text.StringLike
import Text.ParserCombinators.Poly.Lazy

-- | Tag predicate is used to check if tag satisfies given properties and
-- to extract attribute values.  You can compose them using Applicative and
-- Alternative operators: *>, <*, <|> etc.
newtype TagPred s a = TagPred (Tag.Tag s -> Maybe a)

-- | Basic TagPred class instances.
instance Functor (TagPred s) where  
    fmap f (TagPred g) = TagPred $ fmap (fmap f) g

instance Applicative (TagPred s) where  
    pure = TagPred . const . Just
    TagPred f <*> TagPred p = TagPred $ \t -> f t <*> p t

instance Alternative (TagPred s) where  
    empty = TagPred $ \t -> Nothing
    TagPred p <|> TagPred p' = TagPred $ \t -> p t <|> p' t

fromBool :: Bool -> Maybe ()
fromBool True  = Just ()
fromBool False = Nothing

emptyPred :: TagPred s ()
emptyPred = TagPred $ const $ Just ()

isTagOpen :: TagPred s ()
isTagOpen = TagPred (fromBool . Tag.isTagOpen)

isTagClose :: TagPred s ()
isTagClose = TagPred (fromBool . Tag.isTagClose)

isTagOpenName :: Eq s => s -> TagPred s ()
isTagOpenName nm = TagPred (fromBool . Tag.isTagOpenName nm)

-- | Short synonym for isTagOpenName.
tag :: Eq s => s -> TagPred s ()
tag = isTagOpenName

isTagCloseName :: Eq s => s -> TagPred s ()
isTagCloseName nm = TagPred (fromBool . Tag.isTagCloseName nm)

isTagText :: TagPred s ()
isTagText = TagPred (fromBool . Tag.isTagText)

tagOpenName :: TagPred s s
tagOpenName =
    isTagOpen *> TagPred getIt
  where
    getIt (Tag.TagOpen name _) = Just name
    getIt _ = Nothing

hasAttr :: (Show s, Eq s, StringLike s) => s -> s -> TagPred s ()
hasAttr name x =
    isTagOpen *> TagPred checkIt 
  where
    checkIt t = if Tag.fromAttrib name t == x
        then Just ()
        else Nothing

getAttr :: (Show s, Eq s, StringLike s) => s -> TagPred s s
getAttr name =
    isTagOpen *> TagPred getIt
  where
    getIt t = if strNull x
        then Nothing
        else Just x
      where
        x = Tag.fromAttrib name t


-- | XML forest parser with result type a.  TODO: distinguish XmlParser
-- and TagParser types using newtype?
type XmlParser s a = Parser (Tag.Tag s) a
type TagParser s a = Parser (Tag.Tag s) a

ignoreM :: (Applicative f, Monoid m) => f a -> f m
ignoreM f = const mempty <$> f

many_ :: Alternative f => f a -> f ()
many_ v = many_v
  where
    many_v = some_v <|> pure ()
    some_v = v *> many_v

satisfyPred :: TagPred s a -> TagParser s a
satisfyPred (TagPred t) =
    let q = isJust . t
    in  fromJust . t <$> satisfy q

-- | Ignore xml tree or text element. 
ignoreAny :: Eq s => XmlParser s ()
ignoreAny = ignoreText <|> ignoreTag

-- | Ignore text element. 
ignoreText :: XmlParser s ()
ignoreText = satisfyPred isTagText

-- | Ignore xml tree. 
ignoreTag :: Eq s => XmlParser s ()
ignoreTag = do
    name <- satisfyPred tagOpenName
    name `seq` many_ ignoreAny *> satisfyPred (isTagCloseName name)

-- | Versions with monoid result type. 
ignoreTextM :: Monoid m => XmlParser s m
ignoreTextM = ignoreM $ ignoreText

ignoreTagM :: (Eq s, Monoid m) => XmlParser s m
ignoreTagM = ignoreM $ ignoreTag

ignoreAnyM :: (Eq s, Monoid m) => XmlParser s m
ignoreAnyM = ignoreM $ ignoreAny

join :: Eq s => TagPred s a -> XmlParser s b -> XmlParser s (a, b)
join p q = do
    (x, name) <- satisfyPred ((,) <$> p <*> tagOpenName)
    name `seq` x `seq` (x,) <$> q <* satisfyPred (isTagCloseName name)

joinR :: Eq s => TagPred s a -> XmlParser s b -> XmlParser s b
joinR p q = snd <$> join p q

-- joinR :: Eq s => TagPred s a -> XmlParser s b -> XmlParser s b
-- joinR p q = satisfyPred isTagOpen *> q <* satisfyPred isTagClose

joinL :: Eq s => TagPred s a -> XmlParser s b -> XmlParser s a
joinL p q = fst <$> join p q

(/>) :: (Eq s, Monoid m) => TagPred s a -> XmlParser s m -> XmlParser s m
(/>) p q = mconcat <$> joinR p (many $ q <|> ignoreAnyM)
infixr 2 />

(//>) :: (Eq s, Monoid m) => TagPred s a -> TagParser s m -> TagParser s m
(//>) p q = mconcat <$> joinR p (many $ q <|> (emptyPred //> q) <|> ignoreAnyM)
infixr 2 //>

mkGenP :: (Eq s, Monoid m) => (a -> m) -> TagPred s a -> TagParser s m
mkGenP f p = f <$> p `joinL` many_ ignoreAny

mkListP :: Eq s => TagPred s a -> TagParser s [a]
mkListP  = mkGenP (:[])

(/@) :: Eq s => TagPred s a -> TagPred s b -> XmlParser s [b]
(/@) p q = p /> mkListP q
infixr 2 /@

(//@) :: Eq s => TagPred s a -> TagPred s b -> XmlParser s [b]
(//@) p q = p //> mkListP q
infixr 2 //@

mkFirstP :: Eq s => TagPred s a -> TagParser s (First a)
mkFirstP = mkGenP (First . Just)

(/!) :: Eq s => TagPred s a -> TagPred s b -> XmlParser s (First b)
(/!) p q = p /> mkFirstP q
infixr 2 /!

(//!) :: Eq s => TagPred s a -> TagPred s b -> XmlParser s (First b)
(//!) p q = p //> mkFirstP q
infixr 2 //!

-- | Using this function may cause memory leak.  Consider using
-- last <$> ... mkListP ... instead.
mkLastP :: Eq s => TagPred s a -> TagParser s (Last a)
mkLastP = mkGenP (Last . Just)

-- | A shortcut for mkListP.
mkP :: Eq s => TagPred s a -> TagParser s [a]
mkP = mkListP

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

relevant :: StringLike s => Tag.Tag s -> Bool
relevant (Tag.TagText s) = not $ null $ trim $ toString s
relevant _ = True

parseXML :: StringLike s => XmlParser s b -> s -> b
parseXML p xs =
    fst $ runParser p $ filter relevant $ Tag.parseTags xs

-- | Parser example.

-- data Res a = Form a | Sense a deriving (Show)
-- 
-- testParser :: XmlParser String [Res String]
-- testParser = tag "LexicalResource" //> lexParser
-- 
-- lexParser :: XmlParser String [Res String]
-- lexParser = tag "LexicalEntry" />
--         map Form <$> formParser 
--     <|> map Sense <$> senseParser
-- 
-- -- | TODO: There might be two sense relations!
-- senseParser :: XmlParser String [String]
-- senseParser = tag "Sense" `joinR` do
--     x <- optional $ tag "MonolingualExternalRef"
--             /@ hasAttr "att" "externalReference"
--             *> getAttr "val"
--     y <- optional $ tag "SenseRelation"
--             /@ hasAttr "att" "label"
--             *> getAttr "val"
--     many_ ignoreAny
--     choose x y
--   where
--     choose (Just x) _ = return x
--     choose _ (Just y) = return y
--     choose _ _        = error "Sense empty"
-- 
-- formParser :: XmlParser String [String]
-- formParser = tag "WordForm" <|> tag "Lemma"
--           /@ hasAttr "att" "writtenForm" *> getAttr "val"
-- 
-- main = do
--     [inPath] <- getArgs
--     -- xs <- filter relevant . Tag.parseTags <$> readFile inPath
--     xs <- parseXML testParser <$> readFile inPath
--     forM_ xs print
