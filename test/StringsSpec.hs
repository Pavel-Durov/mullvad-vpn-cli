module StringsSpec (
    tests
) where

import Strings (toLower, equalIgnoreCase)
import Test.HUnit


toLowerTestCase = TestCase (
        assertEqual  
        "assertEqual: expected to be equal ignore char case" 
        "jazz" 
        (toLower("JaZZ"))
    )

equalIgnoreCaseTestCase = TestCase (
        assertEqual 
        "equalIgnoreCase: expected to be equal" 
        True
        (equalIgnoreCase "JaZZ"  "jaZz")
    )

equalIgnoreCaseNotEqualTestCase = TestCase (
        assertEqual 
        "assertEqual: expected to be not equal" 
        False
        (equalIgnoreCase "something else"  "jaZz")
    )

tests = TestList [
        TestLabel "toLowerTestCase" toLowerTestCase,
        TestLabel "equalIgnoreCaseTestCase" equalIgnoreCaseTestCase,
        TestLabel "equalIgnoreCaseNotEqualTestCase" equalIgnoreCaseNotEqualTestCase
    ]
