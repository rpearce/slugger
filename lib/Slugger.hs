{-# LANGUAGE OverloadedStrings #-}

module Slugger
    ( toSlugString
    , toSlugText
    ) where


--------------------------------------------------------------------------------


import qualified Data.Char as Char
import qualified Data.Text as T


--------------------------------------------------------------------------------


cleanAmpersand :: T.Text -> T.Text
cleanAmpersand = T.replace "&" "and"


cleanSingleQuote :: T.Text -> T.Text
cleanSingleQuote = T.replace "'" ""


keepAlphaNum :: Char -> Char
keepAlphaNum x
  | Char.isAlphaNum x = x
  | otherwise = ' '


cleanNonAlphaNum :: T.Text -> T.Text
cleanNonAlphaNum = T.map keepAlphaNum


clean :: T.Text -> T.Text
clean = cleanNonAlphaNum . cleanSingleQuote . cleanAmpersand


hyphenateWords :: T.Text -> T.Text
hyphenateWords = T.intercalate (T.singleton '-') . T.words


--------------------------------------------------------------------------------


toSlugText :: T.Text -> T.Text
toSlugText = hyphenateWords . T.toLower . clean


toSlugString :: String -> String
toSlugString = T.unpack . toSlugText . T.pack
