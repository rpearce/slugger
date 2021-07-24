module Data.String.Slugger (toSlug) where


--------------------------------------------------------------------------------
import qualified Data.Text as T
import qualified Data.Text.Slugger as Slugger


--------------------------------------------------------------------------------
{- | Converts to a US-ASCII, lowercase, hyphenated, URI-friendly "slug"

__Examples:__

@
toSlug "Hey there,   world!"
-- "hey-there-world"

toSlug "GARÇON - déjà , Forêt — Zoë"
-- "garcon-deja-foret-zoe"
@
-}
toSlug :: String -> String
toSlug = T.unpack . Slugger.toSlug . T.pack
