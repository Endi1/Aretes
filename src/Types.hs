module Types
  ( Post(..)
  )
where

import           Data.Text

data Post = Post {
    postTitle :: Text,
    markdownPath :: FilePath,
    compilePath :: FilePath
}
