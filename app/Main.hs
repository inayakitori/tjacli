
import Data.List.Split(splitOn, splitOneOf)
import Data.List (intercalate)
import Options.Applicative (Parser)
import Options.Applicative.Builder
import Options.Applicative.Extra
import Control.Applicative ((<**>))
import Lib.Parser (parseArgs, getTJA)

main :: IO ()
main = do
    opts <- parseArgs
    tja <- getTJA opts
    print tja