{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad                  ( void )
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
import           Text.Pandoc
import           GHC.Base                       ( returnIO )
import           GHC.IO.Handle.FD               ( openFile )
import           Text.Blaze.Html5               ( docTypeHtml
                                                , body
                                                , head
                                                , title
                                                , toHtml
                                                )
import           Text.Blaze.Html.Renderer.Text
import qualified Data.Text.IO                  as DTIO
import           Data.Text.Lazy                 ( toStrict )
import           Text.Pandoc.Templates          ( getDefaultTemplate )
import           Text.Pandoc.Walk
import           Text.Pandoc.Shared             ( stringify )
import           GHC.IO                         ( FilePath )
import           System.Directory.Internal.Prelude
                                                ( FilePath )

getPostTitle :: Pandoc -> [Text]
getPostTitle = query titleExtractor
 where
  titleExtractor :: Block -> [Text]
  titleExtractor (Header 1 attr ils) = [stringify ils]
  titleExtractor _                   = []

getPostName :: FilePath -> FilePath
getPostName postFileName = unpack $ dropEnd 3 $ pack postFileName

startCompilingPosts :: IO ()
startCompilingPosts = do
  posts    <- listDirectory "./posts"
  contents <- compilePosts $ map ("./posts/" ++) posts
  print contents

compilePosts :: [FilePath] -> IO ()
compilePosts []       = return ()
compilePosts (x : xs) = do
  readHandle <- openFile x ReadMode
  contents   <- pack <$> hGetContents readHandle
  resultE    <- runIO $ do
    doc <- readMarkdown def contents
    writeHtml5 def doc
  postTitleE <- runIO $ do
    doc <- readMarkdown def contents
    return $ getPostTitle doc

  rst       <- handleError resultE
  postTitle <- handleError postTitleE
  DTIO.writeFile ("./dist/" ++ getPostName x ++ "/" ++ "index.html")
    $ toStrict
    $ renderHtml
    $ docTypeHtml
    $ do
        Text.Blaze.Html5.head $ do
          title $ toHtml $ Prelude.head postTitle
        body rst
  compilePosts xs
 where
  getPostName :: FilePath -> [Char]
  getPostName postFilePath = drop 7 $ unpack $ dropEnd 3 $ pack postFilePath

compilePost :: Text -> IO Text
compilePost postContent = do
  result <- runIO $ do
    doc <- readMarkdown def postContent
    writeRST def doc
  handleError result


createPostsFolders :: IO ()
createPostsFolders = do
  posts <- listDirectory "./posts"
  mapM_ (\filename -> createDirectory $ "./dist/" ++ getPostName filename) posts

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
  createDirectory "./dist/static"
  copyDirectoryRecursive "static" "dist/static" staticRootContents

copyFilesToDist :: IO ()
copyFilesToDist =
  let filesToCopy = ["index.html"]
  in  mapM_ (\filename -> copyFile filename ("./dist/" ++ filename)) filesToCopy

createDistDirectory :: IO ()
createDistDirectory = do
  contents <- listDirectory "."
  if "dist" `elem` contents
    then removeDirectoryRecursive "dist" >> createDirectory "dist"
    else createDirectory "dist"

checkCorrectDirectory :: IO Bool
checkCorrectDirectory = do
  contents <- listDirectory "."
  return $ "aretes.dhall" `elem` contents

main :: IO ()
main = do
  correctDir <- checkCorrectDirectory
  if correctDir
    then
      createDistDirectory
      >> copyFilesToDist
      >> copyStaticFolderContents
      >> createPostsFolders
      >> startCompilingPosts
    else putStrLn "aretes.dhall is missing"
