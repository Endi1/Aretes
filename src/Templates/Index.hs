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
                                                )

import           Types                          ( Post(..) )
import           Control.Monad                  ( forM_ )

index :: [Post] -> Html
index posts = do
  docTypeHtml $ do
    Text.Blaze.Html5.head $ do
      title "Codepenguin"
      meta ! name "viewport" ! content "width=device-width, initial-scale=1.0"
      link ! href "https://cdn.jsdelivr.net/npm/water.css@2/out/dark.css" ! rel
        "stylesheet"
    body $ h1 "Codepenguin"
    ul $ forM_
      posts
      (\post ->
        li $ a ! href (textValue $ postFileName post) $ toHtml $ postTitle post
      )
