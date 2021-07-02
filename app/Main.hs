module Main where


import qualified Data.String.Slugger as Slugger
import qualified System.Environment as Env


help :: String
help =
    "usage: slugger \"<text>\"\n" ++
        "  -h, --help   Read this help info\n"


parse :: [String] -> String
parse ["-h"]     = help
parse ["--help"] = help
parse [str]      = Slugger.toSlug str
parse _          = help


main :: IO ()
main = putStrLn . parse =<< Env.getArgs
