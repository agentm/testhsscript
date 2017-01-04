module Test (Spam(..), SpamFunction, testSpam, testo) where
import Data.Typeable
import System.FilePath.Glob

type SpamFunction = Spam -> Spam
  
data Spam = Spam Int
          deriving (Show, Typeable)
                   
testo :: SpamFunction                   
testo (Spam x) = Spam (x + 1)

testSpam :: Spam
testSpam = Spam 5

anInt :: Int
anInt = 10

globbo :: IO [FilePath]
globbo = glob "*"