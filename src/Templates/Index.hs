{-# LANGUAGE OverloadedStrings #-}
module Templates.Index
  ( index
  )
where

import           Text.Blaze.Html5
import           Text.Blaze.Html5.Attributes    ( name
                                                , content
                                                , href
                                                , rel
                                                , charset
                                                )

import           Types                          ( Post(..) )
import           Control.Monad                  ( forM_ )

import           Templates.Mixins               ( pageHead
                                                , pageFoot
                                                )

index :: [Post] -> Html
index posts = do
  docTypeHtml $ do
    pageHead "Codepenguin"
    body $ h1 "Codepenguin"
    ul $ forM_
      posts
      (\post ->
        li $ a ! href (textValue $ postFileName post) $ toHtml $ postTitle post
      )
    pageFoot
