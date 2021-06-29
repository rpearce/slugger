{-# LANGUAGE OverloadedStrings #-}

module Slugger
    ( toSlugString
    , toSlugText
    ) where


--------------------------------------------------------------------------------
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Text.ICU.Char as ICUChar
import qualified Data.Text.ICU.Normalize as ICUN


--------------------------------------------------------------------------------
toSlugText :: T.Text -> T.Text
toSlugText = hyphenateWords . clean . normalize


toSlugString :: String -> String
toSlugString = T.unpack . toSlugText . T.pack


--------------------------------------------------------------------------------
normalize :: T.Text -> T.Text
normalize = ICUN.normalize ICUN.NFD


--------------------------------------------------------------------------------
clean :: T.Text -> T.Text
clean = T.foldr buildCleanText T.empty


buildCleanText :: Char -> T.Text -> T.Text
buildCleanText x acc
    | isCharModifier x || isSingleQuote x = acc
    | otherwise = T.concat [adjustChar x, acc]


isSingleQuote :: Char -> Bool
isSingleQuote = (== '\'')


isCharModifier :: Char -> Bool
isCharModifier = ICUChar.property ICUChar.Diacritic


adjustChar :: Char -> T.Text
adjustChar 'ø' = "o"
adjustChar 'Ø' = "o"
adjustChar 'ł' = "l"
adjustChar 'Ł' = "l"
adjustChar 'ß' = "ss"
adjustChar x
  | Char.isAlphaNum x = T.singleton (Char.toLower x)
  | otherwise = " "


hyphenateWords :: T.Text -> T.Text
hyphenateWords = T.intercalate (T.singleton '-') . T.words
