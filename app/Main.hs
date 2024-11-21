
import Options.Applicative
import Data.List.Split(splitOn)

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
} deriving (Show)

newtype Course = Course String deriving (Show)

data SongData = SongData {
    title :: String,
    titleja :: String,
    subtitle :: String,
    subtitleja :: String,
    bpm :: Double,
    waveName :: String,
    offset :: Double,
    demoStart :: Double
} deriving (Show)

data ChartData = ChartData {
    course :: String,
    level :: Int,
    scoreInit :: Int,
    scoreDiff :: Int,
    events :: [(Time, GameEvent)]
} deriving (Show)

newtype Time = Time Double  deriving (Show)
data GameEvent = ScrollEvent Double | BPMEvent Double | GogoEvent Bool | NoteEvent  deriving (Show)

parseTJA :: String -> TJAFile
parseTJA tjaData =
    -- head = song info, tail = chart info
    (\(song_text,charts_text) ->
        TJAFile {
            songData = parseSongData song_text,
            charts = (\xs -> (Course (head (words xs)), parseChartData xs)) <$> charts_text
        }
    ) $ (\xs -> (head xs , tail xs)) (splitOn "COURSE:" tjaData)

parseSongData :: String -> SongData
parseSongData songData = error "todo"
parseChartData :: String-> ChartData
parseChartData chartData = error "todo"

main :: IO ()
main = do
    opts <- execParser (info (optionsParser <**> helper)
        ( fullDesc
        <> progDesc "Description"
        <> header "tja-parse" )
        )
    fileContents <- readFile (inputFile opts)
    print (parseTJA fileContents)