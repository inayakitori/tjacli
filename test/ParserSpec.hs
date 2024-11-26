module ParserSpec where

import SpecHelper
import Lib.Parser (emptySongData)

calamityInfoString :: String
calamityInfoString = "\
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
    "Calamity Fortune"
    "Calamity Fortune"
    "--LeaF/Touhou Onsen Yuugi 5"
    "東方Projectアレンジ LeaF"
    200.0
    "Calamity Fortune.ogg"
    (-1.328)
    102.128
    "Calamity Fortune.mp4"



spec:: Spec
spec =
    describe "parseSongData" $ do
        context "[row]" $
            it "should have only filled field of {title = \"The title of the song\"} and the rest unknown" $
                parseSongData ["TITLE:The title of the song"] emptySongData `shouldBe` (emptySongData {title = "The title of the song"})
                
        context "Start of Calamity Fortune file" $
            it "should parse the calamity fortune info" $
                parseSongData (lines calamityInfoString) emptySongData `shouldBe` calamityInfo

main :: IO ()
main = hspec spec