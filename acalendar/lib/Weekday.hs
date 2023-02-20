module Weekday
  ( Weekday (..),
    toString,
  )
where

data Weekday
  = Mon
  | Tue
  | Wed
  | Thu
  | Fri
  | Sat
  | Sun
  deriving (Show, Eq, Ord)

toString :: Weekday -> String
toString Mon = "Mon"
toString Tue = "Tue"
toString Wed = "Wed"
toString Thu = "Thu"
toString Fri = "Fri"
toString Sat = "Sat"
toString Sun = "Sun"
