module File where

import Data.Either
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.IO as Text
import Data.Time.Calendar (Day)
import DateSpec
import Event (Event)
import qualified Event
import Parser
import Text.Parsec (ParseError)
import qualified Text.Parsec as P

data File = File [Either String Event] deriving (Show)

events :: File -> [Event]
events (File xs) = rights xs

readFile :: FilePath -> IO (Either ParseError File)
readFile fp = parse fp <$> Text.readFile fp

parse :: FilePath -> Text -> Either ParseError File
parse fp s = P.parse Parser.file fp s

prune :: Day -> File -> File
prune d (File xs) =
   File
        (filter (either (const True) (isJust . DateSpec.occurrenceAfter d . Event.dateSpec)) xs)

write :: File -> String
write (File xs) =
  unlines (map (either (\s -> "\n;" ++ s ++ "\n") (Event.toString)) xs)
