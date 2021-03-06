module Main (main) where

import Test.Hspec
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
            it "handles Danish: V??re s?? venlig... ??l" $ do
                SluggerText.toSlug (T.pack "V??re s?? venlig... ??l")
                    `shouldBe` T.pack "vaere-sa-venlig-ol"

            it "handles French: GAR??ON - d??j?? , For??t ??? Zo??" $ do
                SluggerText.toSlug (T.pack "GAR??ON - d??j?? , For??t ??? Zo??")
                    `shouldBe` T.pack "garcon-deja-foret-zoe"

            it "handles Finnish: Saisinko leip????? ??akki. D??onkki" $ do
                SluggerText.toSlug (T.pack "Saisinko leip????? ??akki. D??onkki")
                    `shouldBe` T.pack "saisinko-leipaa-sakki-dzonkki"

            it "handles German: Stra??e, m??de, ??pfel und ??kologie" $ do
                SluggerText.toSlug (T.pack "Stra??e, m??de, ??pfel und ??kologie")
                    `shouldBe` T.pack "strasse-mude-apfel-und-okologie"

            it "handles Icelandic: (??g) er kominn aftur (?? n??) Inn ?? ??ig (??a?? er) svo gott a?? vera (h??r) En stoppa stutt vi??" $ do
                SluggerText.toSlug (T.pack "(??g) er kominn aftur (?? n??) Inn ?? ??ig (??a?? er) svo gott a?? vera (h??r) En stoppa stutt vi??")
                    `shouldBe` T.pack "eg-er-kominn-aftur-a-ny-inn-i-thig-thad-er-svo-gott-ad-vera-her-en-stoppa-stutt-vid"

            it "handles Italian: Non c????? di che." $ do
                SluggerText.toSlug (T.pack "Non c????? di che.")
                    `shouldBe` T.pack "non-c-e-di-che"

            it "handles Polish: ????????, Szcz????cie, & Nast??pstw" $ do
                SluggerText.toSlug (T.pack "????????, Szcz????cie, & Nast??pstw")
                    `shouldBe` T.pack "zolc-szczescie-nastepstw"

            it "handles Spanish: ??Qu?? pas??? Soy de Espa??a" $ do
                SluggerText.toSlug (T.pack "??Qu?? pas??? Soy de Espa??a")
                    `shouldBe` T.pack "que-paso-soy-de-espana"

            it "handles Swedish: Vars??god ++ tack s?? mycket -- ??let" $ do
                SluggerText.toSlug (T.pack "Vars??god ++ tack s?? mycket -- ??let")
                    `shouldBe` T.pack "varsagod-tack-sa-mycket-olet"

            it "handles Turkish: Pijamal?? hasta ya????z ??of??re ??abucak g??vendi" $ do
                SluggerText.toSlug (T.pack "Pijamal?? hasta ya????z ??of??re ??abucak g??vendi")
                    `shouldBe` T.pack "pijamali-hasta-yagiz-sofore-cabucak-guvendi"

            it "handles Turkish (uppercase): P??JAMALI HASTA YA??IZ ??OF??RE ??ABUCAK G??VEND??" $ do
                SluggerText.toSlug (T.pack "P??JAMALI HASTA YA??IZ ??OF??RE ??ABUCAK G??VEND??")
                    `shouldBe` T.pack "pijamali-hasta-yagiz-sofore-cabucak-guvendi"

            it "handles a bunch we may have left out: ?? ?? ?? ?? ?? ?? ?? ?? ?? ?? ?? ?? ?? ?? ??" $ do
                SluggerText.toSlug (T.pack "?? ?? ?? ?? ?? ?? ?? ?? ?? ?? ?? ?? ?? ?? ??")
                    `shouldBe` T.pack "ae-ae-d-d-f-f-o-o-oe-oe-l-l-ss-th-th"
