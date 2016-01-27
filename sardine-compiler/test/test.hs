import           Disorder.Core.Main

import qualified Test.Sardine.Compiler.Data

main :: IO ()
main =
  disorderMain [
      Test.Sardine.Compiler.Data.tests
    ]
