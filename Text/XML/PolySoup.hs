{-# LANGUAGE TupleSections #-}

module Text.XML.PolySoup
( XmlParser
, TagPred
, true
, isTagOpen
, isTagClose
, isTagOpenName
, isTagText
, tagOpenName
, tagText
, tag
, hasAttr
, getAttr
, many_
, satisfyPred
, ignoreAny
, ignoreText
, ignoreTag
, ignoreAnyM
, text
, join
, joinR
, joinL
, (</>)
, (/>)
, (</)
, (//>)
, (<#>)
, (#>)
, (##>)
-- , (/@)
-- , (//@)
-- , (/!)
-- , (//!)
-- , mkGenP
-- , mkListP
-- , mkFirstP
-- , mkLastP
-- , mkP
, parseTags
, parseXML
, module Text.ParserCombinators.Poly.Lazy
) where

import System.Environment (getArgs)
import Data.Monoid
import Control.Applicative
import Data.Char (isSpace)
import Control.Monad (forM_)
import Data.Maybe (catMaybes, isJust, fromJust)
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

true :: TagPred s ()
true = pure ()

isTagOpen :: TagPred s ()
isTagOpen = TagPred (fromBool . Tag.isTagOpen)

isTagClose :: TagPred s ()
isTagClose = TagPred (fromBool . Tag.isTagClose)

isTagOpenName :: Eq s => s -> TagPred s ()
isTagOpenName nm = TagPred (fromBool . Tag.isTagOpenName nm)

tagOpenName :: TagPred s s
tagOpenName =
    isTagOpen *> TagPred getIt
  where
    getIt (Tag.TagOpen name _) = Just name
    getIt _ = Nothing

-- | Short synonym for isTagOpenName.
tag :: Eq s => s -> TagPred s ()
tag = isTagOpenName

isTagCloseName :: Eq s => s -> TagPred s ()
isTagCloseName nm = TagPred (fromBool . Tag.isTagCloseName nm)

isTagText :: TagPred s ()
isTagText = TagPred (fromBool . Tag.isTagText)

tagText :: TagPred s s
tagText = TagPred Tag.maybeTagText 

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

-- | Version with monoid result type. 
ignoreAnyM :: (Eq s, Monoid m) => XmlParser s m
ignoreAnyM = const mempty <$> ignoreAny

text :: Eq s => XmlParser s s
text = satisfyPred tagText

join :: Eq s => TagPred s a -> XmlParser s b -> XmlParser s (a, b)
join p q = do
    (x, name) <- satisfyPred ((,) <$> p <*> tagOpenName)
    name `seq` x `seq` (x,) <$> q <* satisfyPred (isTagCloseName name)

joinR :: Eq s => TagPred s a -> XmlParser s b -> XmlParser s b
joinR p q = snd <$> join p q

joinL :: Eq s => TagPred s a -> XmlParser s b -> XmlParser s a
joinL p q = fst <$> join p q

(</>) :: Eq s => TagPred s a -> XmlParser s b -> XmlParser s (a, [b])
(</>) p q =
    join p (catMaybes <$> many qMaybe)
  where
    qMaybe = Just <$> q
         <|> const Nothing <$> ignoreAny
infixr 2 </>

(/>) :: Eq s => TagPred s a -> XmlParser s b -> XmlParser s [b]
(/>) p q = snd <$> (p </> q) -- joinR p (many $ q <|> ignoreAnyM)
infixr 2 />

(</) :: Eq s => TagPred s a -> XmlParser s b -> XmlParser s a
(</) p q = fst <$> (p </> q) -- joinL p (many_ $ q <|> ignoreAnyM)
infixr 2 </

(//>) :: Eq s => TagPred s a -> TagParser s b -> TagParser s [b]
(//>) p q =
    concat <$> joinR p (many qList)
  where
    qList = pure <$> q
        <|> (true //> q)
        <|> ignoreAnyM
infixr 2 //>

-- | Combinators with results concatenation.
(<#>) :: (Eq s, Monoid m) => TagPred s a -> XmlParser s m -> XmlParser s (a, m)
(<#>) p q =
    let mc (x, xs) = (x, mconcat xs)
    in  mc <$> (p </> q)
infixr 2 <#>

(#>) :: (Eq s, Monoid m) => TagPred s a -> XmlParser s m -> XmlParser s m
(#>) p q = mconcat <$> (p /> q)
infixr 2 #>

(##>) :: (Eq s, Monoid m) => TagPred s a -> TagParser s m -> TagParser s m
(##>) p q = mconcat <$> (p //> q)
infixr 2 ##>

-- mkGenP :: (Eq s, Monoid m) => (a -> m) -> TagPred s a -> TagParser s m
-- mkGenP f p = f <$> p `joinL` many_ ignoreAny
-- 
-- mkListP :: Eq s => TagPred s a -> TagParser s [a]
-- mkListP  = mkGenP (:[])
-- 
-- | TODO: write "tag "..." /> tagName </ many_ ignoreAny"
-- | or          "tag "..." /> tagName </ ignore"
-- instead of    "tag "..." /@ tagName" ?
-- (/@) :: Eq s => TagPred s a -> TagPred s b -> XmlParser s [b]
-- (/@) p q = p /> mkListP q
-- infixr 2 /@
-- 
-- (//@) :: Eq s => TagPred s a -> TagPred s b -> XmlParser s [b]
-- (//@) p q = p //> mkListP q
-- infixr 2 //@
-- 
-- mkFirstP :: Eq s => TagPred s a -> TagParser s (First a)
-- mkFirstP = mkGenP (First . Just)
-- 
-- (/!) :: Eq s => TagPred s a -> TagPred s b -> XmlParser s (First b)
-- (/!) p q = p /> mkFirstP q
-- infixr 2 /!
-- 
-- (//!) :: Eq s => TagPred s a -> TagPred s b -> XmlParser s (First b)
-- (//!) p q = p //> mkFirstP q
-- infixr 2 //!
-- 
-- -- | Using this function may cause memory leak.  Consider using
-- -- last <$> ... mkListP ... instead.
-- mkLastP :: Eq s => TagPred s a -> TagParser s (Last a)
-- mkLastP = mkGenP (Last . Just)
-- 
-- -- | A shortcut for mkListP.
-- mkP :: Eq s => TagPred s a -> TagParser s [a]
-- mkP = mkListP

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

relevant :: StringLike s => Tag.Tag s -> Bool
relevant (Tag.TagOpen name _)
    | name == fromString "?xml" = False
    | otherwise = True
relevant (Tag.TagClose _) = True
relevant (Tag.TagText s) = not $ null $ trim $ toString s
relevant _ = False

parseTags :: StringLike s => s -> [Tag.Tag s]
parseTags = filter relevant . Tag.parseTags

parseXML :: StringLike s => XmlParser s b -> s -> b
parseXML p xs
    = fst . runParser p
    . addTop . parseTags $ xs
  where
    addTop xs = Tag.TagOpen topName [] : xs ++ [Tag.TagClose topName]
    topName = fromString "top"

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
