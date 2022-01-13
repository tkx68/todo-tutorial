module Main where

import Relude
import Reflex.Dom
import Data.Text (Text)
import Common

main :: IO ()
main = 
    mainWidgetWithHead -- html element
        headWidget -- head
        rootWidget -- body

bootstrapCSS :: MonadWidget t m => m ()
bootstrapCSS =
  elAttr 
    "link" 
    ( "rel"=:"stylesheet" 
      <> "href"=:"https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css" 
      <> "integrity"=:"sha384-1BmE4kWBq78iYhFldvKuhfTAU6auU8tT94WrHftjDbrCEXSU1oBoqyl2QvZ6jIW3" 
      <> "crossorigin"=:"anonymous")
    blank

bootstrapJS :: MonadWidget t m => m ()
bootstrapJS =
  elAttr 
    "script" 
    ("src"=:"https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.min.js" 
      <> "integrity"=:"sha384-QJHtvGhmr9XOIpI6YVutG+2QOK9T+ZnN4kzFN1RtK3zEFEIsxhlmWl5/YESvpZ13" 
      <> "crossorigin"=:"anonymous")
    blank

icon :: Text
icon = "https://jimdo-storage.freetls.fastly.net/image/128522390/d7a73bb9-c9a2-42f5-ba88-0b675f05f538.png"

headWidget :: MonadWidget t m => m ()
headWidget = do
  elAttr "meta" ("charset" =: "utf-8") blank
  elAttr "meta"
    (  "name" =: "viewport"
    <> "content" =: "width=device-width, initial-scale=1, shrink-to-fit=no" )
    blank
  elAttr "link"
    (  "rel" =: "stylesheet"
    <> "href" =: "https://stackpath.bootstrapcdn.com/bootstrap/4.4.1/css/bootstrap.min.css"
    <> "integrity" =: "sha384-Vkoo8x4CGsO3+Hhxv8T/Q5PaXtkKtu6ug5TOeNV6gBiFeWPGFN9MuhOf23Q9Ifjh"
    <> "crossorigin" =: "anonymous")
    blank
  elAttr "link" ("rel" =: "shortcut icon" <> "href" =: icon <> "type" =: "image/x-icon") blank
  bootstrapCSS
  bootstrapJS
  el "title" $ text "TODO App"

rootWidget :: MonadWidget t m => m ()
rootWidget =
  divClass "container" $ do
    elClass "h2" "text-center mt-3" $ text "Todos"
    newTodoEv <- newTodoForm
    todosDyn <- foldDyn (:) [] newTodoEv
    delimiter
    todoListWidget todosDyn

newTodoForm :: MonadWidget t m => m (Event t Todo)
newTodoForm = rowWrapper $
  el "form" $
    divClass "input-group" $ do
      iEl <- inputElement $ def
        & initialAttributes .~
          (  "type" =: "text"
          <> "class" =: "form-control"
          <> "placeholder" =: "Todo" )
      let
        newTodoDyn = newTodo <$> value iEl
        btnAttr = "class" =: "btn btn-outline-secondary"
          <> "type" =: "button"
      (btnEl, _) <- divClass "input-group-append" $
        elAttr' "button" btnAttr $ text "Add new entry"
      pure $ tagPromptlyDyn newTodoDyn $ domEvent Click btnEl
