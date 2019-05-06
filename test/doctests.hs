import Test.DocTest
import System.Environment

main :: IO ()
main = doctest . getFileFromArgList =<< getArgs

defaultFiles :: [String]
defaultFiles =
  [ "-isrc"
  , "src/Lets/GetSetLens.hs"
  , "src/Lets/Lens.hs"
  , "src/Lets/OpticPolyLens.hs"
  , "src/Lets/StoreLens.hs"
  ]

getFileFromArgList :: [String] -> [String]
getFileFromArgList [] = defaultFiles
getFileFromArgList (x:_) = ["-isrc", x]
