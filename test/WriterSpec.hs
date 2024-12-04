module WriterSpec where 

import SpecHelper
import Lib.Parser (emptySongData)

tja :: TJAFile
tja = TJAFile 
    emptySongData
    []


spec:: Spec
spec =
    describe "SongDataFile" $ do
        context "empty test" $
            it "should be empty" $
                True `shouldBe` True

        
main :: IO ()
main = hspec spec