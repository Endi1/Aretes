module Types
  ( Post(..)
  )
where

import           Data.Text

data Post = Post {
    postFileName :: Text,
    markdownPath :: FilePath,
    compilePath :: FilePath,
    postTitle :: Text,
    compiledPostBody :: Text
}
