module Duration
  ( Duration,
    fromMinutes,
    fromMinutesUnsafe,
    toString,
    plus,
    sum,
  )
where

import Prelude hiding (sum)

newtype Duration = Duration Int
  deriving (Show, Eq)

fromMinutes n
  | n < 0 = Nothing
  | otherwise = Just (Duration n)

fromMinutesUnsafe n
  | n < 0 = error "Duration.fromMinutes: negative"
  | otherwise = Duration n

toString (Duration n) =
  let (h, m) = n `divMod` 60
      pad n cs = if length cs < n then replicate (n - length cs) '0' ++ cs else cs
   in show h ++ ":" ++ pad 2 (show m)

sum :: [Duration] -> Duration
sum = foldr plus (fromMinutesUnsafe 0)

plus :: Duration -> Duration -> Duration
plus (Duration n) (Duration m) =
  Duration (n + m)
