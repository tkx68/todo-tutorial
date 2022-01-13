module Common where

import Data.Time.Calendar
import Relude

data TodoState
  = TodoDone
  | TodoActive {stateEdit :: Bool}
  deriving (Generic, Eq, Show)

data Todo = Todo
  { todoText :: Text,
    todoDeadline :: Day,
    todoState :: TodoState
  }
  deriving (Generic, Eq, Show)

type Todos = IntMap Todo

newTodo :: Text -> Day -> Todo
newTodo todoText todoDeadline = Todo {todoState = TodoActive False, ..}
