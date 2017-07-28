import Test.Hspec

import qualified ApiSpec
import qualified CoreSpec
import qualified SocketSpec
import qualified SubscriptionTreeSpec

main :: IO ()
main = hspec $ do
  ApiSpec.spec
  CoreSpec.spec
  SocketSpec.spec
  SubscriptionTreeSpec.spec
