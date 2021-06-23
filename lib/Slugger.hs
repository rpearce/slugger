{-# LANGUAGE OverloadedStrings #-}

module Slugger (toSlug) where


import qualified Data.Char as Char
import qualified Data.Text as T


keepAlphaNum :: Char -> Char
keepAlphaNum x
  | Char.isAlphaNum x = x
  | otherwise = ' '


clean :: T.Text -> T.Text
clean =
  T.map keepAlphaNum . T.replace "'" "" . T.replace "&" "and"


toSlug :: T.Text -> T.Text
toSlug =
  T.intercalate (T.singleton '-') . T.words . T.toLower . clean
