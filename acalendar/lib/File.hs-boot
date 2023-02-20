module File where

import Event (Event)

data File = File [Either String Event]
