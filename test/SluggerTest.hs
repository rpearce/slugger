module Main (main) where

import Test.Hspec
--import Test.QuickCheck
--import Control.Exception (evaluate)
import qualified Data.Text as T
import qualified Slugger

main :: IO ()
main = hspec $ do
    describe "Slugger.toSlugString" $ do

        context "when given an empty string" $ do
            it "returns an empty string" $ do
                Slugger.toSlugString "" `shouldBe` ""

        context "when given a string with empty spaces" $ do
            it "returns an empty string" $ do
                Slugger.toSlugString "  " `shouldBe` ""

        context "when given uppercase words" $ do
            it "returns words lowercased and hyphenated" $ do
                Slugger.toSlugString "HELLO GREAT WORLD" `shouldBe`
                    "hello-great-world"

        context "when given uppercase words with symbols" $ do
            it "returns words lowercased and hyphenated" $ do
                Slugger.toSlugString "HELLO, WORLD!!!" `shouldBe` "hello-world"


    describe "Slugger.toSlugText" $ do

        context "when given an empty string" $ do
            it "returns an empty string" $ do
                Slugger.toSlugText (T.pack "") `shouldBe` T.pack ""

        context "when given a string with empty spaces" $ do
            it "returns an empty string" $ do
                Slugger.toSlugText (T.pack "  ") `shouldBe` T.pack ""

        context "when given uppercase words" $ do
            it "returns words lowercased and hyphenated" $ do
                Slugger.toSlugText (T.pack "HELLO GREAT WORLD") `shouldBe`
                    T.pack "hello-great-world"

        context "when given uppercase words with symbols" $ do
            it "returns words lowercased and hyphenated" $ do
                Slugger.toSlugText (T.pack "HELLO, WORLD!!!") `shouldBe`
                    T.pack "hello-world"
