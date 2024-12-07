module Lib.Parser (
    parseArgs
    , getTJA
    , parseSongData
    , SongData (..)
    , TJAFile (..)
    , ChartData (..)
    , emptySongData
    , Options (..)
    , GameEvent (..)
    , Note (..)
    ) where

import Data.List (intercalate, isPrefixOf, singleton)
import Options.Applicative (Parser)
import Options.Applicative.Builder
import Options.Applicative.Extra
import Control.Applicative ((<**>))
import Data.Char
import Text.Read (readMaybe)
import Data.Maybe (fromJust, fromMaybe)
import Data.Coerce (coerce)
import Debug.Trace (trace, traceId, traceShowId)
import GHC.IO.IOMode (IOMode(ReadMode))
import GHC.IO.Handle (hSetEncoding, hGetContents)
import System.IO (openFile, utf8, utf8_bom)
import Data.Text (pack, Text, breakOn, unpack)
import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.List.Split as List.Split
import qualified Data.Bifunctor
import Data.Function ((&))

data Options = Options {
    inputFile :: FilePath,
    outputDir :: FilePath,
    verbose :: Bool
} deriving (Show)

optionsParser :: Parser Options
optionsParser = Options
    <$> strOption (long "input" <> short 'i' <> help "The .tja file to parse" <> metavar "FILE")
    <*> strOption (long "output" <> short 'o' <> help "The output osu directory" <> metavar "FOLDER")
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
    charts :: [ChartData]
} deriving (Show, Eq)

data SongData = SongData {
    title :: Text,
    titleja :: Text,
    subtitle :: Text,
    subtitleja :: Text,
    bpm :: Double,
    waveName :: Text,
    offset :: Double,
    demoStart :: Double,
    bgMovie :: Text,
    maker :: Text
} deriving (Show, Eq)

undefText :: Text
undefText = pack "unknown"

emptySongData :: SongData
emptySongData = SongData undefText undefText undefText undefText 0 undefText 0 0 undefText undefText

data ChartData = ChartData {
    course :: Text,
    level :: Int,
    balloon :: Int,
    scoreInit :: Int,
    scoreDiff :: Int,
    events :: [Event]
} deriving (Show, Eq)

type Event = (Double, GameEvent)

emptyChartData :: ChartData
emptyChartData = ChartData undefText (-1) (-1) (-1) (-1) []

data GameEvent =
    ScrollEvent Double
    | BPMEvent Double
    | GogoEvent Bool
    | NoteEvent Note
    | MeasureEvent (Double, Int) -- bpm and measure as X/4
    deriving (Show, Eq)

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
getTJA opts = parseTJA <$> readUTF8File (inputFile opts)

readUTF8File:: FilePath -> IO Text
readUTF8File filepath = pack <$> do
    inputHandle <- openFile filepath ReadMode
    hSetEncoding inputHandle utf8_bom
    hGetContents inputHandle

parseTJA :: Text -> TJAFile
parseTJA tjaData =
    -- head = song info, tail = chart info
    (\(song_text,charts_text) ->
        let
            parsedSongData = parseSongData (Text.lines song_text) emptySongData
        in
        TJAFile {
            songData = parsedSongData,
            charts = (\chartText -> parseChartData parsedSongData chartText emptyChartData)
                <$> tail (splitAtAndKeepDelimiter (pack "COURSE:") charts_text)
        }
    ) $ breakOn (pack "COURSE:") tjaData -- separate

parseSongData :: [Text] -> (SongData -> SongData)
parseSongData [] = id
parseSongData [row]
    | attribute == pack "TITLE" = \s -> s {title = value}
    | attribute == pack "TITLEJA" = \s -> s {titleja = value}
    | attribute == pack "SUBTITLE" = \s -> s {subtitle = value}
    | attribute == pack "SUBTITLEJA" = \s -> s {subtitleja = value}
    | attribute == pack "BPM" = \s -> s {bpm = fromMaybe (-1) (readMaybe (unpack value))}
    | attribute == pack "WAVE" = \s -> s {waveName = value}
    | attribute == pack "OFFSET" = \s -> s {offset =  fromMaybe 0 (readMaybe (unpack value))}
    | attribute == pack "DEMOSTART" = \s -> s {demoStart = fromMaybe 0 (readMaybe (unpack value))}
    | attribute == pack "BGMOVIE" = \s -> s {bgMovie = value}
    | attribute == pack "MAKER" = \s -> s {maker = value}
    | otherwise = id -- need to remove the ":" on it
    where (attribute, value) = breakAndRemoveDelimiter (pack ":")  (Text.strip row)  -- split off the first : but the rest of them should be recombined
parseSongData (row:rows) = parseSongData rows .parseSongData [row]

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
newInterpreter songData = InterpreterState (-offset songData) (bpm songData) (4,4) 1.0 [(-offset songData, BPMEvent (bpm songData))]

-- Slowly "writes" to the ChartData object
parseChartData :: SongData -> Text -> (ChartData -> ChartData)
parseChartData songData chartData = parseChartInfo (Text.lines chartInfo) . \c -> c {events = chartEvents}
    where chartEvents = ievents (parseBars --don't need lyrics
                ((List.Split.splitOn "," . unlines . filter (\ s -> not (s == "" || "#LYRIC" `isPrefixOf` s)) . map unpack . Text.lines) chartEventLines)
                (newInterpreter songData)
            )
          (chartInfo, chartEventLines) = Text.breakOn (pack "#START") chartData 

-- The scroll speed and stuff
parseChartInfo :: [Text] -> (ChartData -> ChartData)
parseChartInfo [] = id
parseChartInfo [row]
    | attribute == pack "COURSE" = \s -> s {course = value}
    | attribute == pack "LEVEL" = \s -> s {level = fromMaybe (-1) (readMaybe (unpack value))}
    | attribute == pack "BALLOON" = \s -> s {balloon = fromMaybe (-1) (readMaybe (unpack value))}
    | attribute == pack "SCOREINIT" = \s -> s {scoreInit = fromMaybe (-1) (readMaybe (unpack value))}
    | attribute == pack "SCOREDIFF" = \s -> s {scoreDiff = fromMaybe (-1) (readMaybe (unpack value))}
    | otherwise = id
    where (attribute, value) = breakAndRemoveDelimiter (pack ":") (Text.strip row) -- split off the first : but the rest of them should be recombined
parseChartInfo (row:rows) = parseChartInfo rows . parseChartInfo [row]

-- The notes and timings. Fed bar sections (separated by commas) and events which get newlines. This one is a little bit different just cause it actually has to use the previous interpreter state
parseBars :: [String] -> InterpreterState -> InterpreterState
parseBars [] i = i -- #ENDBAR is used to indicate increasing time when there are no notes
parseBars [bar] istate = parseBarLines (lines bar ++ ["#ENDBAR"]) subdivision  istate
    where subdivision = length (
            concatMap -- the subdivision is the number of notes in a bar. need to ignore comments too
                (filter isNumber . unpack . fst . Text.breakOn (pack "//") . pack)
                (filter (\ s -> not (s == "" || head s == '#' || "//" `isPrefixOf` s)) (lines bar))
            )
parseBars (bar:bars) istate = parseBars bars (parseBars [bar] istate) -- use the previous interpreter state for this one

-- the lines for each bar. must be read line by line to update the interpreter as it goes
parseBarLines :: [String] -> Int -> InterpreterState -> InterpreterState
parseBarLines [] _ _ = error "should be impossible to have numbers but no text" -- impossible 
parseBarLines ["#ENDBAR"] 0 istate = istate {time = time istate + measureLength} -- no notes, just advance
    where measureLength = 4.0 * 60.0 * measureFraction (measure istate)/ ibpm istate
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
    | "#MEASURE " `isPrefixOf` barLine =
        let (top, btm) = splitAtFirst ' ' barLine
                & snd
                & trim
                & splitAtFirst '/'
                & (\(t,b) -> (fromMaybe 4 (readMaybe t), fromMaybe 4 (readMaybe b)))
                & traceShowId
            requiredBPM = traceShowId $ ibpm istate * fromIntegral btm / 4.0 -- change bpm to make it X/4 at new bpm for timing events
            event = (time istate, MeasureEvent (requiredBPM, top) )
                in addEvent event istate {measure = (top,btm)} -- for the interpreter we can use the actual measure
    | "#DELAY " `isPrefixOf` barLine =
        let new_value = fromMaybe (-1) (readEventValue barLine)
                in istate {time = new_value + time istate} -- the delay just adds on extra time
    | isNumber (head barLine) =
        parseNotes
            (read . singleton <$> barLine)
            (measureLength / fromIntegral subdivision)
            istate
    -- TODO ADD MORE
    | otherwise = istate
    where measureLength = 4.0 * 60.0 * measureFraction (measure istate)/ ibpm istate
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
          timings = (time istate +) . (timePerNote *) . fromIntegral <$> [0 ..(length notes)]
          events =  filter (\e -> snd e /= NoteEvent None) (zip timings notes)

-- drops the #ARG at the start and returns the number (maybe)
readEventValue :: String -> Maybe Double
readEventValue dataline = readMaybe (trim (snd (splitAtFirst ' ' dataline)))

breakAndRemoveDelimiter :: Text -> Text -> (Text, Text)
breakAndRemoveDelimiter delim text = Data.Bifunctor.second Text.tail $ Text.breakOn delim text

addEvent :: Event -> InterpreterState -> InterpreterState
addEvent event i = i {ievents = ievents i ++ [event]}
-- makes the delimiter be attached to start of each element
splitAtAndKeepDelimiter :: Text -> Text -> [Text]
splitAtAndKeepDelimiter delimiter s =
    head_l : (Text.append delimiter <$> tail_l) where
        (head_l, tail_l) = (\l -> (head l, tail l)) $ Text.splitOn delimiter s

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

