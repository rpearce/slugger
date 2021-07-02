module Data.String.Slugger (toSlug) where


--------------------------------------------------------------------------------
import qualified Data.Text as T
import qualified Data.Text.Slugger as Slugger


--------------------------------------------------------------------------------
toSlug :: String -> String
toSlug = T.unpack . Slugger.toSlug . T.pack
