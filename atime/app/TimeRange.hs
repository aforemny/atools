{-# LANGUAGE RecordWildCards #-}

module TimeRange
  ( TimeRange,
    parser,
    toString,
    duration,
  )
where

import Control.Arrow (first)
import Control.Monad (Monad, void)
import Duration (Duration)
import qualified Duration
import Text.Parsec (ParsecT)
import qualified Text.Parsec.Char as P
import qualified Text.Parsec.Combinator as P
import qualified Text.Parsec.Prim as P

-- | Time ranges used in time tracking.
--
-- Time ranges are allowed to have `end` be smaller than `start`, in which case,
-- the time range is interpreted to continue into the next day.
--
-- A time range satisfies:
-- - `(0, 0) <= start, end < ( 24, 60 )`
data TimeRange = TimeRange
  { start :: (Int, Int),
    end :: (Int, Int)
  }
  deriving (Show, Eq, Ord)

parser :: Monad m => ParsecT String u m TimeRange
parser =
  let hour, minute :: Monad m => ParsecT String u m Int
      hour = bounded 24 (read <$> P.count 2 P.digit)
      minute = bounded 60 (read <$> P.count 2 P.digit)

      bounded n p =
        p >>= \x ->
          if x < n
            then pure x
            else P.parserZero

      time :: Monad m => ParsecT String u m (Int, Int)
      time = (,) <$> hour <* void (P.string ":") <*> minute
   in P.try (TimeRange <$> time <* void (P.string "-") <*> time)
        P.<?> "time range"

toString :: TimeRange -> String
toString (TimeRange {..}) =
  let format (h, m) = pad 2 (show h) ++ ":" ++ pad 2 (show m)
      pad n cs = if length cs < n then replicate (n - length cs) '0' ++ cs else cs
   in format start ++ "-" ++ format end

duration :: TimeRange -> Duration
duration (TimeRange {..}) =
  Duration.fromMinutesUnsafe $
    let end' = minutes end
        start' = minutes start
        minutes = uncurry (+) . first (* 60)
     in if start' < end'
          then end' - start'
          else minutes (24, 0) - start' + end'
