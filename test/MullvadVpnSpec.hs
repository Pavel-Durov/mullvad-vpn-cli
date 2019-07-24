module MullvadVpnSpec (
    tests
) where

import Test.HUnit
import  qualified MullvadVpn as VPN


isNotConnectedTestCase = TestCase (
        assertEqual 
        "expected to identify non-connected vpn message" 
        False
        (VPN.isConnectedVpnMessage("You are not connected to Mullvad"))
    )
isConnectedTestCase = TestCase (
        assertEqual 
        "expected to identify connected vpn message" 
        True
        (VPN.isConnectedVpnMessage("You are connected to Mullvad"))
    )

isConnectedStatusTestCase = TestCase (
        assertEqual 
        "expected to identify connected status" 
        True
        (VPN.isConnectedStatus
            VPN.Status { VPN.country = "united kingdom"}
            "united kingdom"
            "You are connected to Mullvad")
    )

isNotConnectedStatusTestCase = TestCase (
        assertEqual 
        "expected to identify connected status" 
        False
        (VPN.isConnectedStatus
            VPN.Status { VPN.country = "united kingdom"}
            "united kingdom"
            "You are NOT connected to Mullvad")
    )

tests = TestList [
        TestLabel "isNotConnected" isNotConnectedTestCase,
        TestLabel "isConnected" isConnectedTestCase,
        TestLabel "isConnectedStatus" isConnectedStatusTestCase,
        TestLabel "isNotConnectedStatus" isNotConnectedStatusTestCase
    ]
