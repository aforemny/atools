{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Char (isSpace)
import Data.Functor (void)
import Data.Maybe (catMaybes)
import Data.Text (Text, pack, unpack)
import Data.Time
import DateSpec (DateSpec (DateSpec))
import Event (Event (Event))
import {-# SOURCE #-} File (File (File))
import Text.Parsec (choice, count, digit, eof, lookAhead, many, many1, newline, noneOf, optionMaybe, satisfy, skipMany, try, (<?>), (<|>))
import qualified Text.Parsec as Parsec
import Text.Parsec.Text
import TimeSpec (TimeSpec (TimeSpec))
import Weekday
import Prelude hiding (until)

file :: Parser File
file =
  File
    <$> ( many
            ( skipMany
                ( void space
                    <|> void newline
                    <|> comment
                )
                *> event
            )
        )
    <* eof

comment :: Parser ()
comment = void (string ";" *> many (noneOf "\n")) <?> "comment"

string :: Text -> Parser Text
string s = pack <$> Parsec.string (unpack s)

space :: Parser Char
space = satisfy (\c -> isSpace c && not (c `elem` ['\r', '\n']))

event :: Parser Event
event =
  Event
    <$> dateSpec
    <*> optionMaybe (token timeSpec)
    <*> literal
    <* (void newline <|> eof)
    <?> "event"

dateSpec :: Parser DateSpec
dateSpec =
  DateSpec
    <$> many (token day)
    <*> many (token month)
    <*> many (token year)
    <*> many (token weekday)
    <*> optionMaybe (token back)
    <*> optionMaybe (token from)
    <*> optionMaybe (token until)
    <?> "date spec"

day :: Parser Int
day = read <$> choice [try (count 2 digit), count 1 digit] <?> "day"

month :: Parser Int
month =
  choice
    ( map
        (\(n, p) -> try (p *> pure n))
        ( zip
            [1 ..]
            [ string "Jan",
              string "Feb",
              string "Mar",
              string "Apr",
              string "May",
              string "Jun",
              string "Jul",
              string "Aug",
              string "Sep",
              string "Oct",
              string "Nov",
              string "Dec"
            ]
        )
    )
    <?> "month"

year :: Parser Int
year = read <$> count 4 digit <?> "year"

weekday :: Parser Weekday
weekday =
  choice
    ( map
        try
        [ string "Mon" *> pure Mon,
          string "Tue" *> pure Tue,
          string "Wed" *> pure Wed,
          string "Thu" *> pure Thu,
          string "Fri" *> pure Fri,
          string "Sat" *> pure Sat,
          string "Sun" *> pure Sun
        ]
    )
    <?> "weekday"

back :: Parser Int
back = read <$> (string "-" *> many1 digit) <?> "back"

from :: Parser Day
from = token (string "FROM") *> date <?> "from"

date :: Parser Day
date =
  fromGregorian
    <$> (read <$> count 4 digit <* string "-")
    <*> (read <$> count 2 digit <* string "-")
    <*> (read <$> count 2 digit)
    <?> "date"

until :: Parser Day
until = token (string "UNTIL") *> date <?> "until"

timeSpec :: Parser TimeSpec
timeSpec = uncurry TimeSpec <$> timeRange <?> "time spec"

timeRange :: Parser (DiffTime, DiffTime)
timeRange = (,) <$> (diffTime <* string "-") <*> diffTime <?> "time range"

literal :: Parser Text
literal =
  pack . catMaybes
    <$> many1
      ( (string "\\\n" *> pure Nothing)
          <|> Just <$> noneOf "\n"
      )
    <?> "literal"

diffTime :: Parser DiffTime
diffTime =
  let hours = read <$> count 2 digit <?> "hours"
      minutes = read <$> count 2 digit <?> "minutes"
   in (\h m -> fromInteger ((h * 60 + m) * 60))
        <$> (hours <* string ":")
        <*> minutes <?> "time"

token :: Parser a -> Parser a
token p = try (p <* endOfToken <* spaces)

endOfToken :: Parser ()
endOfToken = void (lookAhead (space <|> newline)) <|> eof

spaces :: Parser ()
spaces = skipMany space
