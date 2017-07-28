import Test.Hspec

import qualified ApiSpec
import qualified CoreSpec
import qualified PutSpec
import qualified SocketSpec
import qualified SubscriptionTreeSpec

main :: IO ()
main = hspec $ do
  ApiSpec.spec
  CoreSpec.spec
  PutSpec.spec
  SocketSpec.spec
  SubscriptionTreeSpec.spec
