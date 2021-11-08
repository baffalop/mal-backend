import Test.Hspec
import World
  ( new
  , insert
  , Span(..)
  , Serial(..)
  , serialize
  )
import Data.Function ((&))

main :: IO ()
main = hspec $ do
  describe "World" $ do
    it "inserts a layer at an arbitrary positive position" $ do
      (serialize $ insert aspan 5 $ new) `shouldBe`
        Serial
          { _origin = 0
          , _layers = [ [], [], [], [], [], [aspan] ]
          }

    it "inserts a layer at an arbitrary negative position, updating origin" $ do
      (serialize $ insert aspan (-3) $ new) `shouldBe`
        Serial
          { _origin = 3
          , _layers = [ [aspan], [], [], [] ]
          }

    it "tracks origin over multiple inserts" $ do
      (new
        & insert aspan (-1)
        & insert aspan 3
        & insert aspan (-4)
        & insert aspan 0
        & serialize) `shouldBe`
          Serial
            { _origin = 4
            , _layers = [ [aspan], [], [], [aspan], [aspan], [], [], [aspan] ]
            }

    it "appends spans when inserting" $ do
      (new
        & insert aspan 2
        & insert (spanof 0) 2
        & serialize) `shouldBe`
          Serial
            { _origin = 0
            , _layers = [ [], [], [spanof 0, aspan] ]
            }


aspan :: Span
aspan =
  spanof 1

spanof :: Float -> Span
spanof x =
  Span x x x x
