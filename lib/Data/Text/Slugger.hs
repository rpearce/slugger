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
adjustChar 'ç' = "c"
adjustChar 'Ç' = "c"
adjustChar 'ğ' = "g"
adjustChar 'Ğ' = "g"
adjustChar 'ı' = "i"
-- See Note [Turkish I]
adjustChar 'İ' = "i"
adjustChar 'ö' = "o"
adjustChar 'Ö' = "o"
adjustChar 'ş' = "s"
adjustChar 'Ş' = "s"
adjustChar 'ü' = "u"
adjustChar 'Ü' = "u"
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

{-
Note: [Turkish I]

Turkish has an unusual casing of the letter 'I'. In Turkish, we have
'i' and 'ı', two separate letters. They correspond to uppercase 'İ'
and 'I'. Notice that this is the opposite of most other languages,
where lowercase 'i' correspond to uppercase 'I' (losing the dot for no
good reason).

Unicode gets this correctly, so a Unicode-aware `toLower` function would
convert uppercase 'I' to 'ı' when on Turkish locale. This tend to break
functions like the one we are writing, if we incorrectly assume that every
ASCII uppercase letter would correspond to an ASCII lowercase letter.

The surprise is that; `Data.Char.toLower` function we use is not
locale-aware, `Data.Text.ICU.toLower` is. Only because of this fact `I`
becomes `i` even on Turkish locale on this function. This note is here
so that we do not start using `Data.Text.ICU.toLower` and break the
library on Turkish locale.
-}
