{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Arrow hiding ((<+>))
import Control.Monad
import Data.Function ((&))
import Data.List
import Data.List.Extra (breakOn)
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Time
import qualified Data.Tree as T
import Debug.Trace
import Duration (Duration)
import qualified Duration
import Options.Applicative
import Prettyprinter
import Prettyprinter.Render.Terminal
import qualified Storage as S
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Unsafe
import Text.Parsec (ParsecT)
import qualified Text.Parsec.Char as P
import qualified Text.Parsec.Combinator as P
import qualified Text.Parsec.Prim as P
import TimeRange (TimeRange)
import qualified TimeRange

data Args
  = ListArgs
      { includes :: [FilePath]
      }
  | SummaryArgs
      { includes :: [FilePath]
      }
  | TrackArgs
      { includes :: [FilePath],
        base :: Base,
        line :: Line
      }

type Base = String

type Tag = (String, String)

type Project = String

type Annotation = String

data Line = Line Day Project TimeRange Annotation [Tag]
  deriving (Show, Eq, Ord)

toString :: Line -> String
toString (Line day project timeRange label tags) =
  let day' =
        let (y, m, d) = toGregorian day
         in pad 4 (show y) ++ "-" ++ pad 2 (show m) ++ "-" ++ pad 2 (show d)
      timeRange' = TimeRange.toString timeRange
      pad n cs = if length cs < n then replicate (n - length cs) '0' ++ cs else cs
   in intercalate " " $
        [ day',
          project,
          timeRange',
          label
        ]
          ++ map (\(a, b) -> a ++ ":" ++ b) (sort tags)

fromString :: String -> Maybe Line
fromString =
  either (const Nothing) Just . P.parse lineParser ""

lineParser :: Monad m => ParsecT String u m Line
lineParser =
  let dayParser =
        fromGregorian <$> (read <$> P.count 4 P.digit <* P.string "-")
          <*> (read <$> P.count 2 P.digit <* P.string "-")
          <*> (read <$> P.count 2 P.digit)
      projectParser =
        P.many1 (P.noneOf " \r\n")
      stringOrTagsParser =
        partitionTags
          <$> P.many (P.many1 (P.noneOf " \r\n") <* (P.string " " <|> pure ""))
   in uncurry
        <$> ( Line <$> (dayParser <* P.string " ")
                <*> (projectParser <* P.string " ")
                <*> (TimeRange.parser <* P.string " ")
            )
        <*> stringOrTagsParser

linesParser :: Monad m => ParsecT String u m [Line]
linesParser =
  (P.many (lineParser <* P.endOfLine)) <* P.eof

listArgs, summaryArgs, trackArgs :: Parser Args
listArgs = ListArgs <$> includesArg
summaryArgs = SummaryArgs <$> includesArg
trackArgs = TrackArgs <$> includesArg <*> baseArg <*> lineArg

listCmd, summaryCmd, trackCmd :: Mod CommandFields Args
listCmd = command "list" (info listArgs fullDesc)
summaryCmd = command "summary" (info summaryArgs fullDesc)
trackCmd = command "track" (info trackArgs fullDesc)

mainArgs :: ParserInfo Args
mainArgs = info (hsubparser (listCmd <> summaryCmd <> trackCmd) <**> helper) idm

includesArg :: Parser [FilePath]
includesArg = many (strOption (long "include" <> short 'I' <> metavar "DIR"))

baseArg :: Parser Project
baseArg =
  strArgument (metavar "BASE" <> completer baseArgCompleter)

projectArg :: Parser Project
projectArg =
  strArgument (metavar "PROJECT" <> completer projectArgComp)

baseArgCompleter :: Completer
baseArgCompleter =
  mkCompleter $ \input -> do
    includes <- maybe [] (splitOn ":") <$> lookupEnv "ATOOLS_PATH"
    fmap (nub . map (Debug.Trace.traceShowId . head . splitPath . Debug.Trace.traceShowId . S.toString))
      . S.listFiles
      =<< S.open includes

projectArgComp :: Completer
projectArgComp =
  mkCompleter $ \input ->
    do
      includes <- maybe [] (splitOn ":") <$> lookupEnv "ATOOLS_PATH"
      h <- S.open includes
      es <- filter ((==) "time/time" . S.filePath) <$> S.listFiles h
      nub . concat
        <$> mapM
          ( \e -> do
              Just s <- S.readFile h e
              case P.parse linesParser (S.toString e) s of
                Left err -> do
                  hPutStrLn stderr (S.toString e ++ ": " ++ show err)
                  exitWith (ExitFailure 1)
                Right ls -> do
                  pure (map (\(Line _ project _ _ _) -> project) ls)
          )
          es

lineArg :: Parser Line
lineArg =
  ( \day project timeRange ->
      uncurry (Line day project timeRange) . partitionTags
  )
    <$> pure (unsafePerformIO (utctDay <$> getCurrentTime))
    <*> projectArg
    <*> timeRangeArg
    <*> many (strArgument (metavar "LABEL"))

partitionTags :: [String] -> (String, [Tag])
partitionTags ss =
  (intercalate " ") *** map (second tail . breakOn ":") $
    (partition (not . isInfixOf ":") ss)

timeRangeArg :: Parser TimeRange
timeRangeArg =
  argument
    (maybeReader (either (const Nothing) Just . P.parse TimeRange.parser ""))
    (metavar "TIME_RANGE")

main :: IO ()
main = do
  includes' <- maybe [] (splitOn ":") <$> lookupEnv "ATOOLS_PATH"
  execParser mainArgs >>= \case
    TrackArgs {..} -> do
      h <- S.open (includes' ++ includes)
      let e = S.fromString (base </> "time/time")
      s <- (maybe "" id) <$> S.readFile h e
      case P.parse linesParser (S.toString e) s of
        Left e -> do
          hPutStrLn stderr (show e)
          exitWith (ExitFailure 1)
        Right ls -> do
          let s' = unlines (map toString (sort (line : ls)))
          when (s' /= s) $ S.writeFile h e s' >> pure ()
    SummaryArgs {..} ->
      do
        h <- S.open (includes' ++ includes)
        es <- filter ((==) "time/time" . S.filePath) <$> S.listFiles h
        ls <-
          concat
            <$> mapM
              ( \e -> do
                  s <- maybe "" id <$> S.readFile h e
                  case P.parse linesParser (S.toString e) s of
                    Left err -> do
                      hPutStrLn stderr (S.toString e ++ ": " ++ show err)
                      exitWith (ExitFailure 1)
                    Right ls -> do
                      let base = head (splitPath (S.toString e))
                      pure (map ((,) base) ls)
              )
              es
        let perProjectDurations =
              M.unionsWith Duration.plus $
                map
                  ( \(base, (Line _ project timeRange _ _)) ->
                      M.singleton (base </> project) (TimeRange.duration timeRange)
                  )
                  ls
            totalDuration =
              Duration.sum (M.elems perProjectDurations)
        let durations =
              T.unfoldTree
                ( \prefix ->
                    let prefixLen = length prefix
                        reachableDurations =
                          ( M.filterWithKey
                              ( \key _ ->
                                  prefix `isPrefixOf` key
                              )
                              perProjectDurations
                          )
                     in ( ( prefix,
                            Duration.sum (M.elems reachableDurations)
                          ),
                          nub
                            . map ((++) prefix)
                            . catMaybes
                            . map
                              ( \case
                                  [] -> Nothing
                                  (x : _) -> Just x
                              )
                            . map (split (keepDelimsR (onSublist "/")))
                            . filter (/= "")
                            . map (drop prefixLen)
                            . sort
                            $ M.keys reachableDurations
                        )
                )
                ""
        mapM_
          ( \(project, duration) ->
              putStrLn (project ++ " " ++ Duration.toString duration)
          )
          (M.assocs perProjectDurations)
        putStrLn (Duration.toString totalDuration)
        let right s =
              pageWidth
                ( \(AvailablePerLine maxChars ribbonWidth) ->
                    column
                      ( \column ->
                          indent
                            ( floor (fromIntegral maxChars * ribbonWidth)
                                - column
                                - length s
                            )
                            (pretty s)
                      )
                )
        putDoc
          ( T.foldTree
              ( \(prefix, duration) ss ->
                  vsep (pretty prefix <+> right (Duration.toString duration) : map (indent 2) ss)
              )
              durations
          )
    {-
    putDoc
      ( annotate (color Red <> italicized) (pretty "foo")
          <+> annotate (colorDull Red <> bold) (pretty "bar")
      )-}
    ListArgs {..} ->
      do
        h <- S.open (includes' ++ includes)
        es <- filter ((==) "time/time" . S.filePath) <$> S.listFiles h
        ls <-
          concat
            <$> mapM
              ( \e -> do
                  s <- maybe "" id <$> S.readFile h e
                  case P.parse linesParser (S.toString e) s of
                    Left err -> do
                      hPutStrLn stderr (S.toString e ++ ": " ++ show err)
                      exitWith (ExitFailure 1)
                    Right ls -> do
                      pure ls
              )
              es
        putStrLn (unlines (map toString (sort ls)))
