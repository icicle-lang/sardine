import           Disorder.Core.Main

import qualified Test.Sardine.Runtime

main :: IO ()
main =
  disorderMain [
      Test.Sardine.Runtime.tests
    ]
