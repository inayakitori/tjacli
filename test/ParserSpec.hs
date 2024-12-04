module ParserSpec where

import SpecHelper
import Lib.Parser (emptySongData)
import Data.Text (pack, Text)
import qualified Data.Text as Text

calamityInfoString :: Text
calamityInfoString = pack "\
\//TJADB Project \n\
\TITLE:Calamity Fortune \n\
\TITLEJA:Calamity Fortune \n\
\SUBTITLE:--LeaF/Touhou Onsen Yuugi 5 \n\
\SUBTITLEJA:東方Projectアレンジ LeaF\n \
\BPM:200 \n\
\WAVE:Calamity Fortune.ogg\n\
\OFFSET:-1.328\n\n\
\DEMOSTART:102.128\n\
\BGMOVIE:Calamity Fortune.mp4"


calamityInfo :: SongData
calamityInfo = SongData
    (pack "Calamity Fortune")
    (pack "Calamity Fortune")
    (pack "--LeaF/Touhou Onsen Yuugi 5")
    (pack "東方Projectアレンジ LeaF")
    200.0
    (pack "Calamity Fortune.ogg")
    (-1.328)
    102.128
    (pack "Calamity Fortune.mp4")
    (pack "unknown")

spec:: Spec
spec =
    describe "parseSongData" $ do
        context "[row]" $
            it "should have only filled field of {title = \"The title of the song\"} and the rest unknown" $
                parseSongData [pack "TITLE:The title of the song"] emptySongData `shouldBe` (emptySongData {title = pack "The title of the song"})
                
        context "Start of Calamity Fortune file" $
            it "should parse the calamity fortune info" $
                parseSongData (Text.lines calamityInfoString) emptySongData `shouldBe` calamityInfo

main :: IO ()
main = hspec spec