{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


import           Control.Applicative
import           Control.Monad (forM_, void)
import           Text.HTML.TagSoup as S
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import           Text.XML.PolySoup


-- | XML tree specialized to lazy text.
type Tree = XmlTree L.Text


-- | A sentence.
data Sent = Sent
    { sid   :: L.Text
    -- | Token or ns.
    , toks  :: [Maybe Tok] }
    deriving (Show, Eq, Ord)


-- | A sentence token.
data Tok = Tok
    { orth  :: L.Text
    , pos   :: L.Text }
    deriving (Show, Eq, Ord)


-- | Text of the descendant node.
subText :: P Tree L.Text
subText = pop $ node text


-- | A token predicate.
tokQ :: Q Tree Tok
tokQ = named "tok" ^> Tok
    <$> find (named "orth" ^> subText)
    <*> find (named "pos"  ^> subText)


-- | No-space tag.
nsQ :: Q Tree ()
nsQ = void $ node $ named "ns"


-- | A sentence predicate.
sentQ :: Q Tree Sent
sentQ = fmap (uncurry Sent) $
    named "s" *> attr "xml:id" </>
        Just <$> tokQ <|>
        Nothing <$ nsQ


-- | A paragraph predicate.
parQ :: Q Tree [Sent]
parQ = named "p" /> sentQ
    

-- | Parse and render input data. 
main = do
    inp <- L.getContents
    case runQ parQ $ parseTree (S.parseTags inp) of
        Nothing -> putStrLn "No parse"
        Just ss -> forM_ ss $ \Sent{..} -> do
            putStrLn $ "# Sent " ++ L.unpack sid
            mapM_ print toks
