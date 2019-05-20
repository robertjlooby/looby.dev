{-# LANGUAGE OverloadedStrings #-}

import           Data.Time ( getCurrentTime )
import           Data.Time.Format ( defaultTimeLocale, formatTime )
import           System.IO.Unsafe ( unsafePerformIO )

import           Hakyll

main :: IO ()
main = hakyllWith config $ do
    match "public/*" $ do
        route (gsubRoute "public/" (const ""))
        compile copyFileCompiler

    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    match "posts/*" $ do
        route $ setExtension "html"
        compile $
            pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    constField "copyrightYear" copyrightYear
                    <> listField "posts" postCtx (return posts)
                    <> constField "title" "Home"
                    <> defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

config :: Configuration
config =
    defaultConfiguration
    { destinationDirectory = "docs"
    }

postCtx :: Context String
postCtx =
    constField "copyrightYear" copyrightYear
    <> dateField "date" "%B %e, %Y"
    <> defaultContext

copyrightYear :: String
copyrightYear =
    unsafePerformIO $ formatTime defaultTimeLocale "%Y" <$> getCurrentTime

{-# NOINLINE copyrightYear #-}
