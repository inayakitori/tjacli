
import Data.List.Split(splitOn, splitOneOf)
import Data.List (intercalate)
import Options.Applicative (Parser)
import Options.Applicative.Builder
import Options.Applicative.Extra
import Control.Applicative ((<**>))
import Lib.Parser (parseArgs, getTJA, Options (outputDir))
import Text.Pretty.Simple (pPrint)
import Lib.Writer (writeOSU)
import Debug.Trace (traceShow, traceId, traceShowId)
import qualified Data.List.Split.Internals as Data
main :: IO ()
main = do
    opts <- parseArgs
    tja <- getTJA opts 
    writeOSU opts tja