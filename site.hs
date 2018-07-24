{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Data.Monoid ((<>))
import           Hakyll

main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    match (fromList [ "index.md"
                    , "contact.md"
                    , "projects.md"
                    ]) $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler
