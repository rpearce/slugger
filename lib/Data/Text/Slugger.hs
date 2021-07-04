{-# LANGUAGE OverloadedStrings #-}

module Data.Text.Slugger (toSlug) where


--------------------------------------------------------------------------------
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Text.ICU.Char as ICUChar
import qualified Data.Text.ICU.Normalize as ICUN


--------------------------------------------------------------------------------
toSlug :: T.Text -> T.Text
toSlug = hyphenateWords . clean . normalize


--------------------------------------------------------------------------------
normalize :: T.Text -> T.Text
normalize = ICUN.normalize ICUN.NFKD


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
adjustChar 'æ' = "ae"
adjustChar 'Æ' = "ae"
adjustChar 'ð' = "d"
adjustChar 'Ð' = "d"
adjustChar 'ƒ' = "f"
adjustChar 'Ƒ' = "f"
adjustChar 'ø' = "o"
adjustChar 'Ø' = "o"
adjustChar 'œ' = "oe"
adjustChar 'Œ' = "oe"
adjustChar 'ł' = "l"
adjustChar 'Ł' = "l"
adjustChar 'ß' = "ss"
adjustChar 'þ' = "th"
adjustChar 'Þ' = "th"
adjustChar x
  | isAsciiAlphaNum x = toLowerAsText x
  | otherwise = " "


isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum x = Char.isAscii x && Char.isAlphaNum x


toLowerAsText :: Char -> T.Text
toLowerAsText = T.singleton . Char.toLower


--------------------------------------------------------------------------------
hyphenateWords :: T.Text -> T.Text
hyphenateWords = T.intercalate (T.singleton '-') . T.words
