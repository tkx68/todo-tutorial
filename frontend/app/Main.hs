{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Reflex.Dom

main :: IO ()
main = mainWidgetWithHead headWidget rootWidget

headWidget :: MonadWidget t m => m ()
headWidget = blank

rootWidget :: MonadWidget t m => m ()
rootWidget = blank