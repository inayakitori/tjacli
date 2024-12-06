{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use void" #-}
{-# HLINT ignore "Use <&>" #-}
module Lib.Writer (
    writeOSU
    ) where
import Lib.Parser (TJAFile(..), Options (..), SongData (..), ChartData (..), GameEvent (..), Note (..))
import System.FilePath (combine, takeDirectory, (</>), (<.>), takeExtension, makeValid)
import System.Directory (createDirectory, createDirectoryIfMissing, copyFile, listDirectory)
import Data.Text (unpack, Text, pack, concat, intercalate)
import Control.Monad (foldM)
import Data.Coerce (coerce)
import Debug.Trace (traceShowId)
import Data.Text.IO (writeFile)
import qualified Data.List.Split.Internals as Data
import GHC.IO.IOMode (IOMode(WriteMode))
import System.IO (utf8, hPutStr, hSetEncoding, openFile, hClose)
import qualified Data.Text as Text
import Data.List (sortBy)
import qualified Codec.Archive.Zip as Zip
import Data.String (IsString(fromString))
import Codec.Archive.Zip (emptyArchive)
import Data.Time.Clock.System (SystemTime)
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BL




writeOSU :: Options -> TJAFile -> IO ()
writeOSU opts tja = createDirectoryIfMissing True outputFolder
    >> copyFile
        (inputFolder </> (unpack . waveName . songData) tja)
        (outputFolder </> outputAudioName)
    >> mapM
        (\chart ->
            let diffText = unpack $ Data.Text.concat [title (songData tja), pack " [", course chart, pack "]"] in
            do
                outHandle <- openFile (makeValid (outputFolder </> diffText <.> "osu")) WriteMode
                hSetEncoding outHandle utf8
                hPutStr outHandle (chartText (songData tja) chart)
                hClose outHandle
        )
        (charts tja)
    >> return ()
    >> BL.readFile (inputFolder </> (unpack . waveName . songData) tja)
    -- write the archive of all the entries
    >>= \audioFile -> ( BL.writeFile (outputFolder <.> "osz")
        . Zip.fromArchive
        . foldl (flip Zip.addEntryToArchive) emptyArchive
        ) -- the entry an files
        (Zip.toEntry outputAudioName 0 audioFile : (createChartEntry (songData tja) <$> charts tja))
    >> return ()
    where
        outputFolder = outputDir opts </> (makeValid . unpack . title . songData) tja
        inputFolder = takeDirectory (inputFile opts)
        outputAudioName = (unpack . Text.concat) [pack "audio.", snd (Text.breakOnEnd (pack ".") ((waveName . songData) tja))]

createChartEntry :: SongData -> ChartData -> Zip.Entry
createChartEntry sd cd = Zip.toEntry fileName 0 ((TLE.encodeUtf8 . TL.pack) (chartText sd cd))
    where
        diffText = unpack $ Data.Text.concat [title sd, pack " [", course cd, pack "]"]
        fileName = makeValid (diffText <.> "osu")

chartText :: SongData -> ChartData -> String
chartText sd cd =
    "osu file format v14" ++
    "\n\n[General]" ++
    "\nAudioFilename: " ++ (unpack . Text.concat) [pack "audio.", snd (Text.breakOnEnd (pack ".") (waveName sd))] ++
    "\nAudioLeadIn: " ++ (show . secToMs . negate . offset) sd ++
    "\nPreviewTime: " ++ (show . secToMs) (demoStart sd) ++
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
    "\n" ++ getTimingEvents sd cd ++
    "\n\n[HitObjects]" ++
    "\n" ++ getHitEvents sd cd ++
    "\n"

secToMs :: Double -> Int
secToMs = round . (*1000)

getTimingEvents :: SongData -> ChartData -> String
getTimingEvents sd cd = unlines $ snd <$> sortBy (\event1 event2 -> fst event1 `compare` fst event2)
    ( -- get the event text and their timings. should be done differently but I'm not optimising this
        ((\(t,e) -> -- time,beatLength,meter,sampleSet,sampleIndex,volume,uninherited,effects
            (t, (show . secToMs) t ++ "," ++ mapEventValue e ++ ",4,0,0,100,1")
            ) <$> [x | x@(_, BPMEvent _) <- events cd]) ++
        ((\(t,e) ->
            (t, (show . secToMs) t ++ "," ++ mapEventValue e ++ ",4,0,0,100,0")
        ) <$> [x | x@(_, ScrollEvent _) <- events cd]) ++
        ((\(t,e) ->
            let (newBpm, newMeasure) = getMeasure e in
            (t, (show . secToMs) t ++ "," ++ show newBpm ++ "," ++ show newMeasure ++ ",0,0,100,1")
        ) <$> [x | x@(_, MeasureEvent _) <- events cd])
    )


getHitEvents :: SongData -> ChartData -> String
getHitEvents sd cd = unlines (
        (\(t,e) ->
            "0,0," ++ (show . secToMs) t ++ ",1," ++ mapEventValue e
        ) <$> [x | x@(_, NoteEvent _) <- events cd]
    )

getMeasure :: GameEvent -> (Double, Int)
getMeasure (MeasureEvent e) = e
getMeasure _ = error "can't get measure for non-measure event"

mapEventValue :: GameEvent -> String
mapEventValue (NoteEvent Don) = "0"
mapEventValue (NoteEvent BigDon) = "4"
mapEventValue (NoteEvent Ka) = "8"
mapEventValue (NoteEvent BigKa) = "12"
mapEventValue (BPMEvent bpmValue) = show $ 1000.0 * 60.0 / bpmValue
mapEventValue (ScrollEvent scroll) = show (100.0 * (-1.0)/scroll)
mapEventValue val = "%{PARSE NOT IMPLEMENTED: " ++ show val ++ "}%"
