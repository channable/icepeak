import Test.Hspec

import qualified ApiSpec
import qualified CoreSpec
import qualified SocketSpec

main :: IO ()
main = hspec $ do
  ApiSpec.spec
  SocketSpec.spec
  CoreSpec.spec
