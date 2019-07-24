import Test.HUnit
import qualified StringsSpec as Strings
import qualified MullvadVpnSpec as MullvadVpn

main = do
    runTestTT Strings.tests
    runTestTT MullvadVpn.tests
