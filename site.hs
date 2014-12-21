--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           System.FilePath (takeBaseName, replaceExtension, takeFileName)


--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "lib/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler
{-
    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls
-}

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "postSnapshot"
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls
{-
    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)
            rawPosts <- recentFirst =<< loadAll ("posts/*" .&&. hasVersion "raw")
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    listField "rawPosts" postCtx (return rawPosts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= loadAndApplyTemplate "templates/right-panel.html" archiveCtx
                >>= relativizeUrls
-}


    match "posts/*" $ version "raw" $ do
        compile getResourceBody

    create ["right-panel.html"] $ do
        compile $ do
            rawPosts <- recentFirst =<< loadAll ("posts/*" .&&. hasVersion "raw")
            let rightPanelCtx =
                    listField "rawPosts" rawPostCtx (return rawPosts) `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/right-panel.html" rightPanelCtx
                >>= relativizeUrls
{-
    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- fmap (map dropMore) .fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*" "postSnapshot"
            -- posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls
-}
    match "templates/*" $ compile templateCompiler

dropMore :: Item String -> Item String
dropMore = fmap (unlines . takeWhile (/= "<!-- MORE -->") . lines)

config :: Configuration
config = defaultConfiguration
    { deployCommand = "git add . && git commit -m 'new' && git push origin hakyll"
    }

--------------------------------------------------------------------------------

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    listField "posts" defaultContext (loadAll $ "posts/*" .&&. hasNoVersion) `mappend`
    listField "rawPosts" rawPostCtx (loadAll $ "posts/*" .&&. hasVersion "raw") `mappend`
    defaultContext

rawPostCtx :: Context String
rawPostCtx =
    dateField "date" "%B %e, %Y" `mappend`
    (field "url" $ return . (\p -> "/" ++ replaceExtension p "html") . toFilePath . itemIdentifier) `mappend`
    defaultContext
