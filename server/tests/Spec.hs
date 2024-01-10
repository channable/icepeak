import Test.Hspec

import qualified Icepeak.Server.AccessControlSpec
import qualified Icepeak.Server.ApiSpec
import qualified Icepeak.Server.CoreSpec
import qualified Icepeak.Server.JwtSpec
import qualified Icepeak.Server.PersistenceSpec
import qualified Icepeak.Server.RequestSpec
import qualified Icepeak.Server.SocketSpec
import qualified Icepeak.Server.StoreSpec
import qualified Icepeak.Server.SubscriptionTreeSpec
import qualified Icepeak.Server.MultiSubscriptionSpec

main :: IO ()
main = hspec $ do
  Icepeak.Server.AccessControlSpec.spec
  Icepeak.Server.ApiSpec.spec
  Icepeak.Server.CoreSpec.spec
  Icepeak.Server.JwtSpec.spec
  Icepeak.Server.PersistenceSpec.spec
  Icepeak.Server.RequestSpec.spec
  Icepeak.Server.SocketSpec.spec
  Icepeak.Server.StoreSpec.spec
  Icepeak.Server.SubscriptionTreeSpec.spec
  Icepeak.Server.MultiSubscriptionSpec.spec
