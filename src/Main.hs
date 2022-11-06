{-# LANGUAGE OverloadedStrings #-}
module Main where

import           System.Directory               ( listDirectory
                                                , removeDirectoryRecursive
                                                , createDirectory
                                                , copyFile
                                                , doesDirectoryExist
                                                , doesFileExist
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                , dropEnd
                                                , concat
                                                , unpack
                                                )
import           System.IO                      ( FilePath
                                                , openFile
                                                , IOMode(ReadMode, WriteMode)
                                                , hGetContents
                                                )
import           Text.Pandoc                    ( writeRST
                                                , handleError
                                                , writeHtml5
                                                , readMarkdown
                                                , runIO
                                                , def
                                                , Block(Header)
                                                , Pandoc
                                                )
import           GHC.Base                       ( returnIO )
import           GHC.IO.Handle.FD               ( openFile )
import           Text.Blaze.Html.Renderer.Text  ( renderHtml )
import qualified Data.Text.IO                  as DTIO
                                                ( writeFile )
import           Data.Text.Lazy                 ( toStrict )
import           Text.Pandoc.Templates          ( getDefaultTemplate )
import           Text.Pandoc.Walk
import           Text.Pandoc.Shared             ( stringify )
import           GHC.IO                         ( FilePath )
import           System.Directory.Internal.Prelude
                                                ( FilePath )
import           Types                          ( Post(..) )
import           Templates.Index                ( index )
import           Templates.Post                 ( post )
import           Paths_aretes

getPostTitle :: Pandoc -> [Text]
getPostTitle = query titleExtractor
 where
  titleExtractor :: Block -> [Text]
  titleExtractor (Header 1 attr ils) = [stringify ils]
  titleExtractor _                   = []

getPostName :: FilePath -> FilePath
getPostName postFileName = unpack $ dropEnd 3 $ pack postFileName

buildPosts :: IO [Post]
buildPosts = do
  postMarkdownFiles <- listDirectory "./posts"
  compilePosts postMarkdownFiles

compilePosts :: [FilePath] -> IO [Post]
compilePosts []       = return []
compilePosts (p : ps) = do
  readHandle <- openFile ("./posts/" ++ p) ReadMode
  contents   <- pack <$> hGetContents readHandle
  resultE    <- runIO $ do
    doc <- readMarkdown def contents
    writeHtml5 def doc
  postTitleE <- runIO $ do
    doc <- readMarkdown def contents
    return $ getPostTitle doc

  rst         <- handleError resultE
  postTitle   <- handleError postTitleE
  restOfPosts <- compilePosts ps
  return
    $ Post { postFileName     = pack $ getPostName p
           , markdownPath     = "./posts/" ++ p
           , compilePath      = "./docs/" ++ getPostName p
           , postTitle        = Prelude.head postTitle
           , compiledPostBody = post (Prelude.head postTitle) rst
           }

    : restOfPosts

writePosts :: [Post] -> IO ()
writePosts []       = return ()
writePosts (p : ps) = do
  DTIO.writeFile (compilePath p ++ "/" ++ "index.html") (compiledPostBody p)
  writePosts ps


compilePost :: Text -> IO Text
compilePost postContent = do
  result <- runIO $ do
    doc <- readMarkdown def postContent
    writeRST def doc
  handleError result

createPostsFolders :: [Post] -> IO ()
createPostsFolders posts = do
  mapM_ (createDirectory . compilePath) posts

copyDirectoryRecursive :: FilePath -> FilePath -> [FilePath] -> IO ()
copyDirectoryRecursive src dist []                          = return ()
copyDirectoryRecursive src dist (currentFile : restOfFiles) = do
  let currentFileSrcLocation  = src ++ "/" ++ currentFile
      currentFileDistLocation = dist ++ "/" ++ currentFile
  isFile <- doesFileExist currentFileSrcLocation
  if isFile
    then copyFile currentFileSrcLocation currentFileDistLocation
    else do
      let newDist = dist ++ "/" ++ currentFile
          newSrc  = src ++ "/" ++ currentFile
      createDirectory newDist
      srcContents <- listDirectory newSrc
      copyDirectoryRecursive newSrc newDist srcContents

  copyDirectoryRecursive src dist restOfFiles

copyStaticFolderContents :: IO ()
copyStaticFolderContents = do
  staticRootContents <- listDirectory "./static"
  createDirectory "./docs/static"
  copyDirectoryRecursive "static" "docs/static" staticRootContents

compileStaticFiles :: [Post] -> IO ()
compileStaticFiles posts =
  let filesToCompile = [("index.html", index)]
  in  mapM_
        (\(filename, template) ->
          DTIO.writeFile ("./docs/" ++ filename) $ toStrict $ renderHtml
            (index posts)
        )
        filesToCompile

createDistDirectory :: IO ()
createDistDirectory = do
  contents <- listDirectory "."
  if "docs" `elem` contents
    then removeDirectoryRecursive "docs" >> createDirectory "docs"
    else createDirectory "docs"

checkCorrectDirectory :: IO Bool
checkCorrectDirectory = do
  contents <- listDirectory "."
  return $ "aretes.dhall" `elem` contents

main :: IO ()
main = do
  correctDir <- checkCorrectDirectory
  if correctDir
    then do
      createDistDirectory
      createDirectory "./docs/js"
      createDirectory "./docs/css"
      jsFileLocation        <- getDataFileName "resources/ganalytics.js"
      cssFileLocation       <- getDataFileName "resources/bamboo.min.css"
      customCssFileLocation <- getDataFileName "resources/custom.css"
      copyFile jsFileLocation        "./docs/js/ganalytics.js"
      copyFile cssFileLocation       "./docs/css/bamboo.min.css"
      copyFile customCssFileLocation "./docs/css/custom.css"
      copyStaticFolderContents
      posts <- buildPosts
      compileStaticFiles posts
      createPostsFolders posts
      writePosts posts
    else putStrLn "aretes.dhall is missing"
