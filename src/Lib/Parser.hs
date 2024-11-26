module Lib.Parser (
    parseArgs
    , getTJA
    , parseSongData
    , SongData (..)
    , emptySongData
    ) where

import Data.List.Split(splitOn, splitOneOf)
import Data.List (intercalate)
import Options.Applicative (Parser)
import Options.Applicative.Builder
import Options.Applicative.Extra
import Control.Applicative ((<**>))
import Data.Char
import Text.Read (readMaybe)
import Data.Maybe (fromJust, fromMaybe)

data Options = Options {
    inputFile :: FilePath,
    verbose :: Bool
} deriving (Show)

optionsParser :: Parser Options
optionsParser = Options
    <$> strOption (long "input" <> short 'i' <> help "The .tja file to parse" <> metavar "FILE")
    <*> switch (long "verbose" <> short 'v' <> help "verbose output" <> showDefault)

data TJAFile = TJAFile {
    songData :: SongData,
    charts :: [(Course, ChartData)]
} deriving (Show, Eq)

newtype Course = Course String deriving (Show, Eq)

data SongData = SongData {
    title :: String,
    titleja :: String,
    subtitle :: String,
    subtitleja :: String,
    bpm :: Double,
    waveName :: String,
    offset :: Double,
    demoStart :: Double,
    bgMovie :: String
} deriving (Show, Eq)

emptySongData :: SongData
emptySongData = SongData "unknown" "unknown" "unknown" "unknown" 0 "unknown" 0 0 "unknown"

data ChartData = ChartData {
    course :: String,
    level :: Int,
    balloon :: Int,
    scoreInit :: Int,
    scoreDiff :: Int,
    events :: [(Time, GameEvent)]
} deriving (Show, Eq)

emptyChartData :: ChartData
emptyChartData = ChartData "unknown" (-1) (-1) (-1) (-1) []

newtype Time = Time Double  deriving (Show, Eq)
data GameEvent = ScrollEvent Double | BPMEvent Double | GogoEvent Bool | NoteEvent  deriving (Show, Eq)



parseArgs :: IO Options
parseArgs = execParser (info (optionsParser <**> helper)
        ( fullDesc
        <> progDesc "Description"
        <> header "tja-parse" )
        )


getTJA:: Options -> IO TJAFile
getTJA opts = parseTJA <$> readFile (inputFile opts)


parseTJA :: String -> TJAFile
parseTJA tjaData =
    -- head = song info, tail = chart info
    (\(song_text,charts_text) -> let parsedSongData = parseSongData (lines song_text) emptySongData in
        TJAFile {
            songData = parsedSongData,
            charts = (\chartData -> (Course (head (words chartData)), parseChartData parsedSongData chartData emptyChartData)) <$> charts_text
        } 
    ) $ (\xs -> (head xs , tail xs)) (splitAtAndKeepDelimiter "COURSE:" tjaData) -- separate

parseSongData :: [String] -> (SongData -> SongData)
parseSongData [] = id
parseSongData [row]
    | attribute == "TITLE" = \s -> s {title = value}
    | attribute == "TITLEJA" = \s -> s {titleja = value}
    | attribute == "SUBTITLE" = \s -> s {subtitle = value}
    | attribute == "SUBTITLEJA" = \s -> s {subtitleja = value}
    | attribute == "BPM" = \s -> s {bpm = fromMaybe (-1) (readMaybe value)}
    | attribute == "WAVE" = \s -> s {waveName = value}
    | attribute == "OFFSET" = \s -> s {offset =  fromMaybe 0 (readMaybe value)}
    | attribute == "DEMOSTART" = \s -> s {demoStart = fromMaybe 0 (readMaybe value)}
    | attribute == "BGMOVIE" = \s -> s {bgMovie = value}
    | otherwise = id
    where (attribute, value) = (\r -> (trim (head r), intercalate ":" (tail r))) (splitOn ":"  (trim row)) -- split off the first : but the rest of them should be recombined
parseSongData (row:rows) = parseSongData rows . parseSongData [row]

-- as the interpreter goes along the timings will increase and will store the current state information
data InterpreterState = InterpreterState {
    time :: Double,
    ibpm :: Double,
    iscroll :: Double
} deriving (Show, Eq)

newInterpreter :: SongData -> InterpreterState
newInterpreter songData = InterpreterState 0.0 (bpm songData) 1.0

--Slowly "writes" to the ChartData object
parseChartData :: SongData -> String -> (ChartData -> ChartData)
parseChartData _ [] = id
parseChartData songData chartData = parseChartInfo chartInfo . parseChartEvents (newInterpreter songData) chartEvents
    where (chartInfo, chartEvents) = splitAtFirst "#START" (lines chartData)

parseChartInfo :: [String] -> (ChartData -> ChartData)
parseChartInfo [] = id
parseChartInfo [row]
    | attribute == "COURSE" = \s -> s {course = value}
    | attribute == "LEVEL" = \s -> s {level = fromMaybe (-1) (readMaybe value)}
    | attribute == "BALLOON" = \s -> s {balloon = fromMaybe (-1) (readMaybe value)}
    | attribute == "SCOREINIT" = \s -> s {scoreInit = fromMaybe (-1) (readMaybe value)}
    | attribute == "SCOREDIFF" = \s -> s {scoreDiff = fromMaybe (-1) (readMaybe value)}
    | otherwise = id
    where (attribute, value) = (\r -> (trim (head r), intercalate ":" (tail r))) (splitOn ":"  (trim row)) -- split off the first : but the rest of them should be recombined
parseChartInfo (row:rows) = parseChartInfo rows . parseChartInfo [row]

parseChartEvents :: InterpreterState -> [String] -> (ChartData -> ChartData)
parseChartEvents istate chartData = error "todo"

-- makes the delimiter be attached to start of each element
splitAtAndKeepDelimiter :: Eq a => [a] -> [a] -> [[a]]
splitAtAndKeepDelimiter delimiter s =
    head_l : ((delimiter++) <$> tail_l) where
        (head_l, tail_l) = (\l -> (head l, tail l)) $ splitOn delimiter s

splitAtFirst :: Eq a => a -> [a] -> ([a], [a])
splitAtFirst x = fmap (drop 1) . break (x ==)

-- these fns were stolen from https://stackoverflow.com/a/6270382
trim :: [Char] -> String
trim xs = dropSpaceTail "" $ dropWhile isSpace xs

dropSpaceTail :: [Char] -> [Char] -> String
dropSpaceTail _ "" = ""
dropSpaceTail maybeStuff (x:xs)
        | isSpace x = dropSpaceTail (x:maybeStuff) xs
        | null maybeStuff = x : dropSpaceTail "" xs
        | otherwise       = reverse maybeStuff ++ x : dropSpaceTail "" xs

