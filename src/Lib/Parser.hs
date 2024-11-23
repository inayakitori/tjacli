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
    scoreInit :: Int,
    scoreDiff :: Int,
    events :: [(Time, GameEvent)]
} deriving (Show, Eq)

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
    (\(song_text,charts_text) ->
        TJAFile {
            songData =  parseSongData (lines song_text) emptySongData,
            charts = (\xs -> (Course (head (words xs)), parseChartData xs)) <$> charts_text
        }
    ) $ (\xs -> (head xs , tail xs)) (splitOn "COURSE:" tjaData)

parseSongData :: [String] -> (SongData -> SongData)
parseSongData [] = id
parseSongData [row]
    | attribute == "TITLE" = \s -> s {title = value}
    | attribute == "TITLEJA" = \s -> s {titleja = value}
    | attribute == "SUBTITLE" = \s -> s {subtitle = value}
    | attribute == "SUBTITLEJA" = \s -> s {subtitleja = value}
    | attribute == "BPM" = \s -> s {bpm = read value}
    | attribute == "WAVE" = \s -> s {waveName = value}
    | attribute == "OFFSET" = \s -> s {offset = read value}
    | attribute == "DEMOSTART" = \s -> s {demoStart = read value}
    | attribute == "BGMOVIE" = \s -> s {bgMovie = value}
    | otherwise = id
    where (attribute, value) = (\r -> (trim (head r), intercalate ":" (tail r))) (splitOn ":"  (trim row)) -- split off the first : but the rest of them should be recombined
parseSongData (row:rows) = parseSongData rows . parseSongData [row]
parseChartData :: String-> ChartData
parseChartData chartData = error "todo"

-- these fns were stolen from https://stackoverflow.com/a/6270382
trim :: [Char] -> String
trim xs = dropSpaceTail "" $ dropWhile isSpace xs

dropSpaceTail :: [Char] -> [Char] -> String
dropSpaceTail _ "" = ""
dropSpaceTail maybeStuff (x:xs)
        | isSpace x = dropSpaceTail (x:maybeStuff) xs
        | null maybeStuff = x : dropSpaceTail "" xs
        | otherwise       = reverse maybeStuff ++ x : dropSpaceTail "" xs