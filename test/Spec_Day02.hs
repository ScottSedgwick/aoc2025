module Spec_Day02 (day02) where
    
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Day02

day02 :: [Test.Framework.Test]
day02 = 
  [ testBad2 "11" True
  ]

testBad2 :: String -> Bool -> Test.Framework.Test
testBad2 s b = testCase ("Day 02, Bad2: " <> s) (isBad2 s @?= b)