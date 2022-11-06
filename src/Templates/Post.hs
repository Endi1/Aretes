module Templates.Post
  ( post
  )
where

import           Text.Blaze.Html5               ( toHtml
                                                , body
                                                , link
                                                , meta
                                                , title
                                                , head
                                                , docTypeHtml
                                                , (!)
                                                , Html
                                                )
import           Text.Blaze.Html5.Attributes    ( name
                                                , content
                                                , href
                                                , rel
                                                )
import           Data.Text                      ( Text )
import           Data.Text.Lazy                 ( toStrict )
import           Text.Blaze.Html.Renderer.Text  ( renderHtml )


import           Templates.Mixins               ( pageHead
                                                , pageFoot
                                                )

post :: Text -> Html -> Text
post postTitle postBody = toStrict $ renderHtml $ docTypeHtml $ do
  pageHead postTitle
  body $ do
    postBody
    pageFoot
