module GHCJS where

import Data.Text (Text)
import GHCJS.DOM (currentDocumentUnchecked)
import GHCJS.DOM.Document
  ( createElement,
    execCommand,
    getBodyUnchecked,
  )
import GHCJS.DOM.Element as Element hiding (scroll)
import GHCJS.DOM.HTMLElement as HE (focus)
import GHCJS.DOM.HTMLInputElement as HIE (select, setValue)
import GHCJS.DOM.Node (appendChild, removeChild)
import GHCJS.DOM.Types hiding (Event, Text)
import Reflex.Dom
import Relude

-- | This following JavaScript function is the equivalent of adding the following JS to an event handler:
--
--     function toClipboard(txt){
--       var inpEl = document.createElement("textarea");
--       document.body.appendChild(inpEl);
--       inpEl.value = txt
--       inpEl.focus();
--       inpEl.select();
--       document.execCommand('copy');
--       document.body.removeChild(inpEl);
--     }
toClipboard :: MonadJSM m => Text -> m ()
toClipboard txt = do
  doc <- currentDocumentUnchecked
  body <- getBodyUnchecked doc
  -- var inpEl = document.createElement("textarea")
  inpEl <- uncheckedCastTo HTMLInputElement <$> createElement doc ("textarea" :: Text)
  -- document.body.appendChild(inpEl)
  void $ appendChild body inpEl
  -- inpEl.focus()
  HE.focus inpEl
  -- inpEl.value = txt
  HIE.setValue inpEl txt
  -- inpEl.select()
  HIE.select inpEl
  -- document.execCommand('copy')
  void $ execCommand doc ("copy" :: Text) False (Nothing :: Maybe Text)
  -- document.body.removeChild(inpEl)
  void $ removeChild body inpEl

-- | Bind `toClipboard` to some event.
copyByEvent :: MonadWidget t m => Text -> Event t () -> m ()
copyByEvent txt ev =
  void $
    performEvent $ -- bind the handler to the event
      ev $> toClipboard txt -- replace content of `ev` with `toClipboard txt`
