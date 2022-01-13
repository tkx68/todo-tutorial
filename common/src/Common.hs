module Common where

import Relude

data TodoState
  = TodoDone
  | TodoActive {stateEdit :: Bool}
  deriving (Generic, Eq, Show)

data Todo = Todo
  { todoText :: Text,
    todoState :: TodoState
  }
  deriving (Generic, Eq, Show)

type Todos = IntMap Todo

newTodo :: Text -> Todo
newTodo todoText = Todo {todoState = TodoActive False, ..}