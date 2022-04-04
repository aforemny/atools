module File where

import qualified Data.Text.IO as Text
import Event (Event)
import  Parser
import Text.Parsec (ParseError)
import Text.Parsec (parse)

data File = File {events :: [Event]} deriving (Show)

readFile :: FilePath -> IO (Either ParseError File)
readFile fp = parse Parser.file fp <$> Text.readFile fp
