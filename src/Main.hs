{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad                  ( void )
import           System.Directory               ( listDirectory
                                                , removeDirectoryRecursive
                                                , createDirectory
                                                , copyFile
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
import           Text.Pandoc.Lua                ( Global(PANDOC_SCRIPT_FILE) )
import qualified Data.Text.IO                  as DTIO

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
  result     <- runIO $ do
    doc <- readMarkdown def contents
    writeHtml5String def doc
  rst <- handleError result
  DTIO.writeFile ("./dist/" ++ getPostName x ++ "/" ++ "index.html") rst
  return ()
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
      >> createPostsFolders
      >> startCompilingPosts
    else putStrLn "aretes.dhall is missing"
