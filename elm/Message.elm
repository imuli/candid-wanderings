module Message exposing (Message(..))

import Keyboard exposing (KeyCode)

type Message
  = NoOp
  | KeyMsg KeyCode

