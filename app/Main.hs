
import Data.List.Split(splitOn, splitOneOf)
import Data.List (intercalate)
import Options.Applicative (Parser)
import Options.Applicative.Builder
import Options.Applicative.Extra
import Control.Applicative ((<**>))
import Lib.Parser (parseArgs, getTJA)
import Text.Pretty.Simple (pPrint)
main :: IO ()
main = do
    opts <- parseArgs
    tja <- getTJA opts
    pPrint tja