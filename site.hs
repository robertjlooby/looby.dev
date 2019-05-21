{-# LANGUAGE OverloadedStrings #-}

import           Data.Time ( getCurrentTime )
import           Data.Time.Format ( defaultTimeLocale, formatTime )
import           System.IO.Unsafe ( unsafePerformIO )

import           Hakyll

main :: IO ()
main = hakyllWith config $ do
    match "public/**" $ do
        route (gsubRoute "public/" (const ""))
        compile copyFileCompiler

    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    match "posts/*" $ do
        route $ setExtension "html"
        compile $
            pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" postContext
            >>= loadAndApplyTemplate "templates/default.html" postContext
            >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexContext =
                    baseContext <> listField "posts" postContext (return posts)

            getResourceBody
                >>= applyAsTemplate indexContext
                >>= loadAndApplyTemplate "templates/default.html" indexContext
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

config :: Configuration
config =
    defaultConfiguration
    { destinationDirectory = "docs"
    }

postContext :: Context String
postContext = baseContext <> dateField "date" "%B %e, %Y"

baseContext :: Context String
baseContext = defaultContext <> constField "copyrightYear" copyrightYear

copyrightYear :: String
copyrightYear =
    unsafePerformIO $ formatTime defaultTimeLocale "%Y" <$> getCurrentTime

{-# NOINLINE copyrightYear #-}
