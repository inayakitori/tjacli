module Lib.Writer (
    writeOSU
    ) where
import Lib.Parser (TJAFile(..), Options (..), SongData (..), ChartData (..), GameEvent (..), Note (..))
import System.FilePath (combine, takeDirectory, (</>), (<.>))
import System.Directory (createDirectory, createDirectoryIfMissing, copyFile)
import Data.Text (unpack, Text, pack, concat, intercalate)
import Control.Monad (foldM)
import Data.Coerce (coerce)
import Debug.Trace (traceShowId)
import Data.Text.IO (writeFile)
import qualified Data.List.Split.Internals as Data
import GHC.IO.IOMode (IOMode(WriteMode))
import System.IO (utf8, hPutStr, hSetEncoding, openFile, hClose)


writeOSU :: Options -> TJAFile -> IO ()
writeOSU opts tja = createDirectoryIfMissing True outputFolder >>
    mapM
        (\(difficulty, chart) ->
            let diffText = unpack $ Data.Text.concat [title (songData tja), pack " [", difficulty, pack "]"] in
            do
                outHandle <- openFile (outputFolder </> diffText <.> "osu") WriteMode
                hSetEncoding outHandle utf8
                hPutStr outHandle (chartText (songData tja) chart)
                hClose outHandle                
        )
        (charts tja)
    >> return ()
    where
        outputFolder = outputDir opts </> unpack (title $ songData tja)
        inputFolder = takeDirectory (inputFile opts)

chartText :: SongData -> ChartData -> String
chartText sd cd =
    "osu file format v14" ++
    "\n\n[General]" ++
    "\nAudioFilename: " ++ unpack (waveName sd) ++
    "\nAudioLeadIn: " ++ (show . secToMs . offset) sd ++
    "\nPreviewTime: " ++ show (demoStart sd) ++
    "\nStackLeniency: 0.7" ++
    "\nMode: 1" ++
    "\nCountdown: 0" ++
    "\nSampleSet: Normal" ++
    "\n\n[Metadata]" ++
    "\nTitle:" ++ unpack (titleja sd) ++
    "\nTitleUnicode:" ++ unpack (title sd) ++
    "\nCreator:" ++ unpack (maker sd) ++
    "\nVersion:" ++ unpack (course cd) ++
    "\nSource: TJA conversion" ++
    "\nTags: tja_convert" ++
    "\n\n[Difficulty]" ++
    "\nHPDrainRate:7" ++
    "\nCircleSize:5" ++
    "\nOverallDifficulty:5.5" ++
    "\nApproachRate:5" ++
    "\nSliderMultiplier:1.4" ++
    "\nSliderTickRate:1" ++
    "\n\n[TimingPoints]" ++
    "\n" ++ getBPMEvents sd cd ++
    "\n\n[HitObjects]" ++
    "\n" ++ getHitEvents sd cd ++
    "\n"

secToMs :: Double -> Int
secToMs = round . (*1000)

getBPMEvents :: SongData -> ChartData -> String
getBPMEvents sd cd = "todo! bpmevent"


getHitEvents :: SongData -> ChartData -> String
getHitEvents sd cd = unlines (
    (\(t,e) -> 
        "0,0," ++ (show . secToMs) t ++ ",1," ++ show ( mapNoteEvent e)
    ) <$> [x | x@(_, NoteEvent _) <- events cd] 
    )

    
mapNoteEvent :: GameEvent -> Int
mapNoteEvent (NoteEvent Don) = 0
mapNoteEvent (NoteEvent BigDon) = 4
mapNoteEvent (NoteEvent Ka) = 8
mapNoteEvent (NoteEvent BigKa) = 12
mapNoteEvent _ = -1
