module Main (main) where

import Test.Hspec
--import Test.QuickCheck
--import Control.Exception (evaluate)
import qualified Data.Text as T
import qualified Data.String.Slugger as SluggerString
import qualified Data.Text.Slugger as SluggerText

main :: IO ()
main = hspec $ do
    describe "Data.String.Slugger.toSlug" $ do

        it "works" $ do
            SluggerString.toSlug "HELLO, WORLD!!!" `shouldBe` "hello-world"


    describe "Data.Text.Slugger.toSlug" $ do

        context "when an empty string" $ do
            it "returns an empty string" $ do
                SluggerText.toSlug (T.pack "") `shouldBe` T.pack ""

        context "when a string with empty spaces" $ do
            it "returns an empty string" $ do
                SluggerText.toSlug (T.pack "  ") `shouldBe` T.pack ""

        context "when uppercase words" $ do
            it "returns words lowercased and hyphenated" $ do
                SluggerText.toSlug (T.pack "HELLO GREAT WORLD") `shouldBe`
                    T.pack "hello-great-world"

        context "when uppercase words with symbols" $ do
            it "returns words lowercased and hyphenated" $ do
                SluggerText.toSlug (T.pack "HELLO, WORLD!!!") `shouldBe`
                    T.pack "hello-world"

        context "when contractions are used (single quotes)" $ do
            it "keeps words like can't and don't together" $ do
                SluggerText.toSlug (T.pack "I can't won't don't want to!")
                    `shouldBe` T.pack "i-cant-wont-dont-want-to"

        context "when leading & trailing whitespace" $ do
            it "does nothing with the extra whitespace" $ do
                SluggerText.toSlug (T.pack "  Hey world!   ") `shouldBe`
                    T.pack "hey-world"

        context "when repeated inner whitespace" $ do
            it "treats the repeated inner whitespace as a single space" $ do
                SluggerText.toSlug (T.pack "Hey there,  world!") `shouldBe`
                    T.pack "hey-there-world"

        context "when non US-ASCII letters provided" $ do
            it "handles Spanish: ¿Qué pasó? Soy de España" $ do
                SluggerText.toSlug (T.pack "¿Qué pasó? Soy de España")
                    `shouldBe` T.pack "que-paso-soy-de-espana"

            it "handles Polish: Żółć, Szczęście, & Następstw" $ do
                SluggerText.toSlug (T.pack "Żółć, Szczęście, & Następstw")
                    `shouldBe` T.pack "zolc-szczescie-nastepstw"

            it "handles German: Straße, müde, Äpfel und Ökologie" $ do
                SluggerText.toSlug (T.pack "Straße, müde, Äpfel und Ökologie")
                    `shouldBe` T.pack "strasse-mude-apfel-und-okologie"

            it "handles French: GARÇON - déjà , Forêt — Zoë" $ do
                SluggerText.toSlug (T.pack "GARÇON - déjà , Forêt — Zoë")
                    `shouldBe` T.pack "garcon-deja-foret-zoe"

            it "handles Swedish: Varsågod ++ tack så mycket -- ölet" $ do
                SluggerText.toSlug (T.pack "Varsågod ++ tack så mycket -- ölet")
                    `shouldBe` T.pack "varsagod-tack-sa-mycket-olet"

            it "handles Icelandic: (Ég) er kominn aftur (á ný) Inn í þig (Það er) svo gott að vera (hér) En stoppa stutt við" $ do
                SluggerText.toSlug (T.pack "(Ég) er kominn aftur (á ný) Inn í þig (Það er) svo gott að vera (hér) En stoppa stutt við")
                    `shouldBe` T.pack "eg-er-kominn-aftur-a-ny-inn-i-thig-thad-er-svo-gott-ad-vera-her-en-stoppa-stutt-vid"

            it "handles a bunch we left out: æ Æ ð Ð ƒ Ƒ ø Ø œ Œ ł Ł ß þ Þ" $ do
                SluggerText.toSlug (T.pack "æ Æ ð Ð ƒ Ƒ ø Ø œ Œ ł Ł ß þ Þ")
                    `shouldBe` T.pack "ae-ae-d-d-f-f-o-o-oe-oe-l-l-ss-th-th"
