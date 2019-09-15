--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith myConfig $ do
    match "images/*" $ do
        route idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    match pages $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

myConfig :: Configuration
myConfig =
    defaultConfiguration { deployCommand = "cp -r _site/* .." }

pages :: Pattern
pages = fromList
    [ "index.markdown"
    , "contact.markdown"
    , "projects.markdown"
    ]

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
