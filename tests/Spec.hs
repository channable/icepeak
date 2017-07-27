import Test.Hspec

import qualified ApiSpec
import qualified SocketSpec
import qualified PutSpec

main :: IO ()
main = hspec $ do
  ApiSpec.spec
  SocketSpec.spec
  PutSpec.spec
