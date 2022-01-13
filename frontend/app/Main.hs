module Main where

import Common
  ( Todo (..),
    TodoState (TodoActive, TodoDone),
    Todos,
    newTodo,
  )
import Control.Lens ((&), (.~))
import Data.IntMap (maxViewWithKey, update)
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Text (Text, pack, unpack)
import Data.Time
  ( Day,
    UTCTime (utctDay),
    defaultTimeLocale,
    formatTime,
    getCurrentTime,
    parseTimeM,
  )
import GHC.Generics (Generic)
import GHCJS (copyByEvent)
import JSFFI (addDatePicker)
import Reflex.Dom
  ( DomBuilder (inputElement),
    EventName (Click),
    EventWriter (..),
    HasDomEvent (domEvent),
    HasValue (value),
    InitialAttributes (initialAttributes),
    MonadWidget,
    PostBuild (getPostBuild),
    Reflex (Dynamic),
    blank,
    def,
    divClass,
    dyn,
    el,
    elAttr,
    elAttr',
    elClass,
    ffor,
    foldDyn,
    holdUniqDyn,
    inputElementConfig_initialValue,
    inputElementConfig_setValue,
    leftmost,
    listWithKey,
    mainWidgetWithHead,
    runEventWriterT,
    tagPromptlyDyn,
    text,
    (=:),
  )
import Reflex.Dom.Contrib.Widgets.ScriptDependent
  ( widgetHoldUntilDefined,
  )
import Relude
import Relude.Extra.Map

data TodoEvent
  = NewTodo Todo
  | ToggleTodo Int
  | StartEditTodo Int
  | FinishEditTodo (Text, Int)
  | DeleteTodo Int
  deriving (Generic, Eq, Show)

main :: IO ()
main =
  mainWidgetWithHead -- html element
    headWidget -- head
    rootWidget -- body

bootstrapCSS :: MonadWidget t m => m ()
bootstrapCSS =
  elAttr
    "link"
    ( "rel" =: "stylesheet"
        <> "href" =: "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css"
        <> "integrity" =: "sha384-1BmE4kWBq78iYhFldvKuhfTAU6auU8tT94WrHftjDbrCEXSU1oBoqyl2QvZ6jIW3"
        <> "crossorigin" =: "anonymous"
    )
    blank

bootstrapJS :: MonadWidget t m => m ()
bootstrapJS =
  elAttr
    "script"
    ( "src" =: "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.min.js"
        <> "integrity" =: "sha384-QJHtvGhmr9XOIpI6YVutG+2QOK9T+ZnN4kzFN1RtK3zEFEIsxhlmWl5/YESvpZ13"
        <> "crossorigin" =: "anonymous"
    )
    blank

icon :: Text
icon = "https://jimdo-storage.freetls.fastly.net/image/128522390/d7a73bb9-c9a2-42f5-ba88-0b675f05f538.png"

iconW :: MonadWidget t m => m ()
iconW = elAttr "link" ("rel" =: "shortcut icon" <> "href" =: icon <> "type" =: "image/x-icon") blank

flatPickr :: MonadWidget t m => m ()
flatPickr = do
  elAttr "link" ("rel" =: "stylesheet" <> "href" =: "https://cdn.jsdelivr.net/npm/flatpickr/dist/flatpickr.min.css") blank
  elAttr "script" ("src" =: "https://cdn.jsdelivr.net/npm/flatpickr") blank

headWidget :: MonadWidget t m => m ()
headWidget = do
  elAttr "meta" ("charset" =: "utf-8") blank
  elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1, shrink-to-fit=no") blank
  iconW
  bootstrapCSS
  bootstrapJS
  flatPickr
  el "title" $ text "TODO App"

rootWidget :: MonadWidget t m => m ()
rootWidget =
  divClass "container" $ mdo
    elClass "h2" "text-center mt-3" $ text "Todos"
    (_, ev) <- runEventWriterT do
      todosDyn <- foldDyn appEndo mempty ev
      newTodoForm
      delimiter
      todoListWidget todosDyn
    blank

updateTodo :: TodoEvent -> Todos -> Todos
updateTodo ev todos = case ev of
  NewTodo todo -> nextKey todos =: todo <> todos
  ToggleTodo idx -> update (Just . toggleTodo) idx todos
  StartEditTodo idx -> update (Just . startEdit) idx todos
  FinishEditTodo (v, idx) -> update (Just . finishEdit v) idx todos
  DeleteTodo idx -> delete idx todos

startEdit :: Todo -> Todo
startEdit todo = todo {todoState = TodoActive True}

finishEdit :: Text -> Todo -> Todo
finishEdit val todo =
  todo
    { todoState = TodoActive False,
      todoText = val
    }

toggleTodo :: Todo -> Todo
toggleTodo Todo {..} = Todo {todoState = toggleState todoState, ..}
  where
    toggleState = \case
      TodoDone -> TodoActive False
      TodoActive _ -> TodoDone

nextKey :: IntMap Todo -> Int
nextKey = maybe 0 (succ . fst . fst) . maxViewWithKey

newTodoForm :: (EventWriter t (Endo Todos) m, MonadWidget t m) => m ()
newTodoForm = rowWrapper $
  el "form" $
    divClass "input-group" $ mdo
      iEl <-
        inputElement $
          def
            & initialAttributes .~ ("type" =: "text" <> "class" =: "form-control" <> "placeholder" =: "Todo")
            & inputElementConfig_setValue .~ ("" <$ btnEv)
      dEl <- inputElement $ def & initialAttributes .~ ("type" =: "text" <> "class" =: "form-control" <> "placeholder" =: "Deadline" <> "style" =: "max-width: 150px")
      pb <- getPostBuild
      void $
        widgetHoldUntilDefined -- will build the element passed to it in the last parameter only when the defined script has already been downloaded.
          "flatpickr"
          (pb $> "https://cdn.jsdelivr.net/npm/flatpickr")
          blank
          (addDatePicker dEl)
      today <- utctDay <$> liftIO getCurrentTime
      let dateStrDyn = value dEl
          dateDyn =
            fromMaybe today
              . parseTimeM
                True
                defaultTimeLocale
                "%Y-%m-%d"
              . unpack
              <$> dateStrDyn
          addNewTodo = \todo date -> Endo $ \todos -> insert (nextKey todos) (newTodo todo date) todos
          newTodoDyn = addNewTodo <$> value iEl <*> dateDyn
          btnAttrs = "class" =: "btn btn-outline-secondary" <> "type" =: "button"
      (btnEl, _) <- divClass "input-group-append" $ elAttr' "button" btnAttrs $ text "Add new entry"
      let btnEv = domEvent Click btnEl
      tellEvent $ tagPromptlyDyn newTodoDyn $ domEvent Click btnEl

todoListWidget :: (EventWriter t (Endo Todos) m, MonadWidget t m) => Dynamic t Todos -> m ()
todoListWidget todosDyn =
  rowWrapper $
    void $ listWithKey (M.fromAscList . IM.toAscList <$> todosDyn) todoWidget

-- Optimised version with EventWriter
todoWidget :: (EventWriter t (Endo Todos) m, MonadWidget t m) => Int -> Dynamic t Todo -> m ()
todoWidget idx todoDyn' = do
  -- What’s going on here? The matter is that though the Dynamic operates, the value it
  -- contains remains unchanged. Function `holdUniqDyn` works just this way, so that if the
  -- Dynamic passed to it operates and hasn’t changed its value, the output Dynamic won’t
  -- operate and, consequently, in our case, the DOM won’t be rebuilt unnecessarily.
  todoDyn <- holdUniqDyn todoDyn'
  void $
    dyn $ -- dyn function updates the DOM every time the todoDyn is updated
      ffor todoDyn $ \Todo {..} ->
        case todoState of
          TodoDone -> todoDone idx todoText todoDeadline
          TodoActive False -> todoActive idx todoText todoDeadline
          TodoActive True -> todoEditable idx todoText

rowWrapper :: MonadWidget t m => m a -> m a
rowWrapper ma =
  divClass "row justify-content-md-center" $
    divClass "col-6" ma

delimiter :: MonadWidget t m => m ()
delimiter = rowWrapper $ divClass "border-top mt-3" blank

todoActive :: (EventWriter t (Endo Todos) m, MonadWidget t m) => Int -> Text -> Day -> m ()
todoActive idx todoText todoDeadline = divClass "d-flex border-bottom" do
  elClass "p" "p-2 flex-grow-1 my-auto" do
    text todoText
    elClass "span" "badge badge-secondary px-2" $
      text $ pack $ formatTime defaultTimeLocale "%F" todoDeadline
  divClass "p-2 btn-group" do
    (copyEl, _) <- elAttr' "button" ("class" =: "btn btn-outline-secondary" <> "type" =: "button") $ text "Copy"
    (doneEl, _) <- elAttr' "button" ("class" =: "btn btn-outline-secondary" <> "type" =: "button") $ text "Done"
    (editEl, _) <- elAttr' "button" ("class" =: "btn btn-outline-secondary" <> "type" =: "button") $ text "Edit"
    (delEl, _) <- elAttr' "button" ("class" =: "btn btn-outline-secondary" <> "type" =: "button") $ text "Drop"
    copyByEvent todoText $ domEvent Click copyEl
    tellEvent $
      Endo
        <$> leftmost
          [ update (Just . toggleTodo) idx <$ domEvent Click doneEl,
            update (Just . startEdit) idx <$ domEvent Click editEl,
            delete idx <$ domEvent Click delEl
          ]

todoDone :: (EventWriter t (Endo Todos) m, MonadWidget t m) => Int -> Text -> Day -> m ()
todoDone idx todoText deadline =
  divClass "d-flex border-bottom" do
    elClass "p" "p-2 flex-grow-1 my-auto" do
      text todoText
      elClass "span" "badge badge-secondary px-2" $
        text $ pack $ formatTime defaultTimeLocale "%F" deadline
    divClass "p-2 btn-group" do
      (doneEl, _) <- elAttr' "button" ("class" =: "btn btn-outline-secondary" <> "type" =: "button") $ text "Undo"
      (delEl, _) <- elAttr' "button" ("class" =: "btn btn-outline-secondary" <> "type" =: "button") $ text "Drop"
      tellEvent $
        Endo
          <$> leftmost
            [ update (Just . toggleTodo) idx <$ domEvent Click doneEl,
              delete idx <$ domEvent Click delEl
            ]

todoEditable :: (EventWriter t (Endo Todos) m, MonadWidget t m) => Int -> Text -> m ()
todoEditable idx todoText = divClass "d-flex border-bottom" do
  updTodoDyn <-
    divClass "p-2 flex-grow-1 my-auto" $
      editTodoForm todoText
  divClass "p-2 btn-group" do
    (doneEl, _) <- elAttr' "button" ("class" =: "btn btn-outline-secondary" <> "type" =: "button") $ text "Finish edit"
    let updTodos = \todo -> Endo $ update (Just . finishEdit todo) idx
    tellEvent $
      tagPromptlyDyn (updTodos <$> updTodoDyn) (domEvent Click doneEl)

editTodoForm :: MonadWidget t m => Text -> m (Dynamic t Text)
editTodoForm todo = do
  editIEl <-
    inputElement $
      def
        & initialAttributes
          .~ ( "type" =: "text"
                 <> "class" =: "form-control"
                 <> "placeholder" =: "Todo"
             )
        & inputElementConfig_initialValue .~ todo
  pure $ value editIEl
