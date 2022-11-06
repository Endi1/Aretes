{-# LANGUAGE OverloadedStrings #-}
module Templates.Mixins
  ( pageHead
  , pageFoot
  )
where

import           Text.Blaze.Html5
import           Text.Blaze.Html5.Attributes    ( name
                                                , content
                                                , href
                                                , rel
                                                , charset
                                                , type_
                                                , src
                                                , async
                                                )
import           Data.Text

pageHead :: Text -> Html
pageHead pageTitle = Text.Blaze.Html5.head $ do
  title $ toHtml pageTitle
  meta ! charset "UTF-8"
  meta ! name "viewport" ! content "width=device-width, initial-scale=1.0"
  link ! href "/css/bamboo.min.css" ! rel "stylesheet"
  link ! href "/css/custom.css" ! rel "stylesheet"
  meta ! name "description" ! content "Codepenguin -- code and other stuff"
  link ! rel "canonical" ! href "http://codepengu.in"
  script
    ! type_ "text/javascript"
    ! src "https://www.googletagmanager.com/gtag/js?id=G-FL3V8GHGKZ"
    $ do
        ""
  script ! type_ "text/javascript" ! src "/js/ganalytics.js" $ do
    ""

pageFoot :: Html
pageFoot = Text.Blaze.Html5.head $ do
  footer $ do
    ul $ do
      li $ a ! href "https://twitter.com/codepengu1n" $ "Twitter"
      li $ a ! href "mailto:endisukaj@gmail.com" $ "Email"
