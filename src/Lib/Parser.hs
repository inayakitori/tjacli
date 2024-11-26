module Lib.Parser (
    parseArgs
    , getTJA
    , parseSongData
    , SongData (..)
    , emptySongData
    ) where

import Data.List.Split(splitOn, splitOneOf)
import Data.List (intercalate, isPrefixOf, singleton)
import Options.Applicative (Parser)
import Options.Applicative.Builder
import Options.Applicative.Extra
import Control.Applicative ((<**>))
import Data.Char
import Text.Read (readMaybe)
import Data.Maybe (fromJust, fromMaybe)
import Data.Coerce (coerce)
import Debug.Trace (trace)

data Options = Options {
    inputFile :: FilePath,
    verbose :: Bool
} deriving (Show)

optionsParser :: Parser Options
optionsParser = Options
    <$> strOption (long "input" <> short 'i' <> help "The .tja file to parse" <> metavar "FILE")
    <*> switch (long "verbose" <> short 'v' <> help "verbose output" <> showDefault)

{-

file
    song info
,   [chart data]
        chart info
    ,   [bars]
            [barlines]
                [note]
            |   event

-}


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
    events :: [Event]
} deriving (Show, Eq)

type Event = (Double, GameEvent)

emptyChartData :: ChartData
emptyChartData = ChartData "unknown" (-1) (-1) (-1) (-1) []

data GameEvent = ScrollEvent Double | BPMEvent Double | GogoEvent Bool | NoteEvent Note deriving (Show, Eq)

data Note = None |
    Don | Ka |
    BigDon | BigKa |
    Roll | BigRoll |
    Balloon | EndRoll | BigBalloon
        deriving (Show, Eq, Enum)

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

-- as the interpreter goes along the timings will increase and will store the current state information and events so far
data InterpreterState = InterpreterState {
    time :: Double,
    ibpm :: Double,
    measure :: (Int, Int),
    iscroll :: Double,
    ievents :: [Event]
} deriving (Show, Eq)


measureFraction :: (Int, Int) -> Double
measureFraction m = fromIntegral (fst m) / fromIntegral (snd m)

newInterpreter :: SongData -> InterpreterState
newInterpreter songData = InterpreterState (- offset songData) (bpm songData) (4,4) 1.0 []

-- Slowly "writes" to the ChartData object
parseChartData :: SongData -> String -> (ChartData -> ChartData)
parseChartData _ [] = id -- load the chart info and the song data in 
parseChartData songData chartData = parseChartInfo chartInfo . \c -> c {events = chartEvents}
    where chartEvents = ievents (parseBars (splitOn "," (intercalate "\n" chartEventLines)) (newInterpreter songData))
          (chartInfo, chartEventLines) = splitAtFirst "#START" (lines chartData)

-- The scroll speed and stuff
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

-- The notes and timings. Fed bar sections (separated by commas) and events which get newlines. This one is a little bit different just cause it actually has to use the previous interpreter state
parseBars :: [String] -> InterpreterState -> InterpreterState
parseBars [] i = i
parseBars [bar] istate = parseBarLines (lines bar) subdivision  istate
    where subdivision = length (concatMap -- the subdivision is the number of notes in a bar
                (filter isNumber)
                (filter (\s -> not (s == "" || head s == '#')) (lines bar))
            )
parseBars (bar:bars) istate = parseBars bars (parseBars [bar] istate) -- use the previous interpreter state for this one

-- the lines for each bar. must be read line by line to update the interpreter as it goes
parseBarLines :: [String] -> Int -> InterpreterState -> InterpreterState
parseBarLines [] _ istate = istate -- this would only happen if the bar somehow had no lines
parseBarLines [""] _ istate = istate -- ignore empty lines
parseBarLines [barLine] subdivision istate
    | "#SCROLL " `isPrefixOf` barLine = 
        let new_value = fromMaybe (-1) (readEventValue barLine)
            event = (time istate, ScrollEvent new_value)
                in addEvent event istate {iscroll = new_value} 
    | "#BPMCHANGE " `isPrefixOf` barLine = 
        let new_value = fromMaybe (-1) (readEventValue barLine)
            event = (time istate, BPMEvent new_value)
                in addEvent event istate {ibpm = new_value} 
    | "#DELAY " `isPrefixOf` barLine = 
        let new_value = fromMaybe (-1) (readEventValue barLine)
                in istate {time = new_value + time istate} --the delay just adds on extra time
    | isNumber (head barLine) =
        parseNotes
            (read . singleton <$> barLine)
            (4.0 * 60.0 * measureFraction (measure istate)/ (ibpm istate * fromIntegral subdivision))
            istate
    -- TODO ADD MORE
    | otherwise = istate
parseBarLines (barLine:barLines) subdivision istate =
    parseBarLines barLines subdivision (parseBarLines [barLine] subdivision istate)

-- Takes the notes and the values to parse them and move the interpreter forward
parseNotes :: [Int] -> Double -> InterpreterState -> InterpreterState
parseNotes noteData timePerNote istate =
    istate {
        ievents = ievents istate ++ events,
        time = time istate + timePerNote * fromIntegral (length notes)
    }
    where notes = NoteEvent . toEnum <$> noteData :: [GameEvent]
          timings = (time istate +) . (timePerNote *) . fromIntegral <$> [1 ..(length notes)]
          events =  filter (\e -> snd e /= NoteEvent None) (zip timings notes)

-- drops the #ARG at the start and returns the number (maybe)
readEventValue :: String -> Maybe Double
readEventValue dataline = readMaybe (trim (snd (splitAtFirst ' ' dataline)))  

addEvent :: Event -> InterpreterState -> InterpreterState
addEvent event i = i {ievents = ievents i ++ [event]}
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

