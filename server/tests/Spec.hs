import Test.Hspec

import qualified AccessControlSpec
import qualified ApiSpec
import qualified CoreSpec
import qualified JwtSpec
import qualified SocketSpec
import qualified StoreSpec
import qualified SubscriptionTreeSpec

main :: IO ()
main = hspec $ do
  AccessControlSpec.spec
  ApiSpec.spec
  CoreSpec.spec
  JwtSpec.spec
  SocketSpec.spec
  StoreSpec.spec
  SubscriptionTreeSpec.spec
