{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

module Main where

import Prelude hiding (FilePath)
import System.FilePath
import Data.Maybe (maybe, listToMaybe)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (MonadThrow, throwM, Exception)
import Data.Typeable
import qualified CMark
import CMark (commonmarkToNode)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Text (Text, append, pack)
import Control.Arrow ((>>>))
import Lucid

data ProcessError = NodeNotFound CMark.NodeType | ListNotFound deriving (Show, Typeable)
instance Exception ProcessError

main :: IO ()
main = processMenu >>= (\header -> sequence_ [processIndex header])

-- configure
mainStylesheet :: Text
mainStylesheet = "style.css"
footer :: Html ()
footer = p_ [] "Copyright 2017 S.Percentage all rights reserved."
menuSrc, indexSrc :: FilePath
menuSrc = "menu.md"; indexSrc = "index.md"
outdir :: FilePath
outdir = "dist"

-- utils
println :: (MonadIO m, MonadThrow m) => Text -> m ()
println = liftIO . T.putStrLn
failIfNothing :: (MonadThrow m, Exception e) => e -> Maybe a -> m a
failIfNothing = flip maybe return . throwM
takeFirst :: (MonadThrow m, Exception e) => e -> [a] -> m a
takeFirst = (>>>) listToMaybe . failIfNothing
takeFirstOr :: (MonadThrow m, Exception e) => [a] -> e -> m a
takeFirstOr = flip takeFirst

processMenu :: (MonadIO m, MonadThrow m) => m (Html ())
processMenu = do
  println $ T.concat ["processing ", pack menuSrc, "..."]
  md <- liftIO $ parseMarkdown menuSrc
  heading <- indexLink . nodeText <$> extractHeadings 1 md `takeFirstOr` NodeNotFound (CMark.HEADING 1)
  -- println $ append "site caption: " (pack $ show heading)
  items <- fmap convertToLink <$> (fmap extractListText . listItems) <$> takeFirst ListNotFound (extractTopLists md)
  -- println $ append "menu items: " $ pack . show $ items
  return $ do heading; ul_ [] $ mapM_ (li_ []) items
  -- liftIO $ renderToFile "dist/menu.html.template" doc

processIndex :: (MonadIO m, MonadThrow m) => Html () -> m ()
processIndex header = do
  println $ T.concat ["processing ", pack indexSrc, "..."]
  CMark.Node _ CMark.DOCUMENT (CMark.Node _ (CMark.HEADING _) [CMark.Node _ (CMark.TEXT title) _]:md) <- liftIO $ parseMarkdown indexSrc
  let body = div_ [id_ "container"] $ do header_ [] header; main_ [] $ mapM_ renderNodes md; footer_ [] footer
  let outpath = outdir </> indexSrc -<.> "html"
  println $ T.concat ["  --> ", pack outpath]
  liftIO $ renderToFile outpath $ constructCommonHtml title body

convertToLink, indexLink :: Text -> Html ()
convertToLink t = a_ [href_ (append t ".html")] $ toHtml t
indexLink t = a_ [href_ "index.html"] $ h1_ [] $ toHtml t

renderNodes :: CMark.Node -> Html ()
renderNodes (CMark.Node _ (CMark.TEXT t) _) = toHtml t
renderNodes (CMark.Node _ CMark.PARAGRAPH children) = p_ [] $ mapM_ renderNodes children
renderNodes (CMark.Node _ _ children) = mapM_ renderNodes children
constructCommonHtml :: Text -> Html () -> Html ()
constructCommonHtml pagetitle body = doctypehtml_ $ do
  head_ [] $ do meta_ [charset_ "utf-8"]; link_ [href_ mainStylesheet, rel_ "stylesheet"]; title_ [] $ toHtml pagetitle
  body_ [] body

parseMarkdown :: String -> IO CMark.Node
parseMarkdown = (commonmarkToNode [CMark.optNormalize] <$>) . T.readFile

extractHeadings :: Int -> CMark.Node -> [CMark.Node]
extractTopLists, extractParagraph :: CMark.Node -> [CMark.Node]
extractHeadings l n = case n of CMark.Node _ (CMark.HEADING l') _ | l== l' -> return n; CMark.Node _ _ children -> concatMap (extractHeadings l) children
extractTopLists   n = case n of CMark.Node _ (CMark.LIST _)     _          -> return n; CMark.Node _ _ children -> concatMap extractTopLists     children
extractParagraph  n = case n of CMark.Node _ CMark.PARAGRAPH    _          -> return n; CMark.Node _ _ children -> concatMap extractParagraph    children
nodeText :: CMark.Node -> Text
nodeText (CMark.Node _ _ children) = T.concat $ maybe [] id (mapM extractor children) where
  extractor :: CMark.Node -> Maybe Text
  extractor (CMark.Node _ (CMark.TEXT t) _) = Just t
  extractor _ = Nothing
listItems :: CMark.Node -> [CMark.Node]
listItems (CMark.Node _ (CMark.LIST _) children) = maybe [] id $ mapM extractor children where
  extractor :: CMark.Node -> Maybe CMark.Node
  extractor n@(CMark.Node _ CMark.ITEM _) = Just n
  extractor _ = Nothing
extractListText :: CMark.Node -> Text
extractListText = T.concat . fmap nodeText . extractParagraph
