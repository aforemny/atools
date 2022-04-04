module File where

import Event (Event)

data File = File {events :: [Event]}
