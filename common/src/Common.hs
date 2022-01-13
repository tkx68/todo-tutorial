module Common where

import Relude

newtype Todo = Todo
  { todoText :: Text }

newTodo :: Text -> Todo
newTodo todoText = Todo {..}