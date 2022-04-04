module TimeSpec where

import qualified Data.Text as Text
import Data.Text (Text)
import Data.Time.Clock (DiffTime)
import Text.Printf (printf)

data TimeSpec = TimeSpec
  { start :: DiffTime,
    end :: DiffTime
  }
  deriving (Show)

toString :: TimeSpec -> String
toString (TimeSpec start end) =
  let diffTimeToString s =
        let (h, m) = (floor s `div` 60) `divMod` (60 :: Int)
         in printf "%.2d:%.2d" h m :: String
   in printf "%s-%s" (diffTimeToString start) (diffTimeToString end)

toText :: TimeSpec -> Text
toText = Text.pack . toString
