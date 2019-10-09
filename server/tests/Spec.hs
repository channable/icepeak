import Test.Hspec

import qualified AccessControlSpec
import qualified ApiSpec
import qualified CoreSpec
import qualified JwtSpec
import qualified PersistenceSpec
import qualified RequestSpec
import qualified SocketSpec
import qualified StoreSpec
import qualified SubscriptionTreeSpec

main :: IO ()
main = hspec $ do
  AccessControlSpec.spec
  ApiSpec.spec
  CoreSpec.spec
  JwtSpec.spec
  PersistenceSpec.spec
  RequestSpec.spec
  SocketSpec.spec
  StoreSpec.spec
  SubscriptionTreeSpec.spec
