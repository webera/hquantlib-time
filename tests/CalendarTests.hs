module Main (main) where

import System.Exit (exitFailure, exitSuccess)
import Test.HUnit

import Data.Time.Calendar (fromGregorian)
import QuantLib.Time.Calendars.UnitedStates (martinLutherDay, presidentsDay, memorialDay, laborDay, thanksgivingDay)

testMartinLutherDay :: Test
testMartinLutherDay = TestCase (let martinLutherDays = [ martinLutherDay 2024 == fromGregorian 2024 1 15,
                                                         martinLutherDay 2023 == fromGregorian 2023 1 16,
                                                         martinLutherDay 2022 == fromGregorian 2022 1 17,
                                                         martinLutherDay 2021 == fromGregorian 2021 1 18,
                                                         martinLutherDay 2020 == fromGregorian 2020 1 20 ]
                                    nonMartinLutherDays = [ martinLutherDay 2024 == fromGregorian 2024 1 16,
                                                            martinLutherDay 2023 == fromGregorian 2023 1 17,
                                                            martinLutherDay 2022 == fromGregorian 2022 1 16,
                                                            martinLutherDay 2021 == fromGregorian 2021 1 19,
                                                            martinLutherDay 2020 == fromGregorian 2020 1 21 ]
                                 in do
                                   assertBool "testMartinLutherDay" $ and martinLutherDays
                                   assertBool "testNonMartinLutherDay" $ not $ or nonMartinLutherDays)

testPresidentDay :: Test
testPresidentDay = TestCase (let presidentDayDays = [ presidentsDay 2024 == fromGregorian 2024 2 19,
                                                      presidentsDay 2023 == fromGregorian 2023 2 20,
                                                      presidentsDay 2022 == fromGregorian 2022 2 21,
                                                      presidentsDay 2021 == fromGregorian 2021 2 15,
                                                      presidentsDay 2020 == fromGregorian 2020 2 17 ]
                                 nonPresidentDayDays = [ presidentsDay 2024 == fromGregorian 2024 2 20,
                                                         presidentsDay 2023 == fromGregorian 2023 2 19,
                                                         presidentsDay 2022 == fromGregorian 2022 2 20,
                                                         presidentsDay 2021 == fromGregorian 2021 2 19,
                                                         presidentsDay 2020 == fromGregorian 2020 2 18 ]
                                 in do
                                   assertBool "testPresidentDay" $ and presidentDayDays
                                   assertBool "testNonPresidentDay" $ not $ or nonPresidentDayDays)


testMemorialDay :: Test
testMemorialDay = TestCase (let memorialDayDays = [ memorialDay 2024 == fromGregorian 2024 5 27,
                                                    memorialDay 2023 == fromGregorian 2023 5 29,
                                                    memorialDay 2022 == fromGregorian 2022 5 30,
                                                    memorialDay 2021 == fromGregorian 2021 5 31,
                                                    memorialDay 2020 == fromGregorian 2020 5 25 ]
                                nonMemorialDayDays = [ memorialDay 2024 == fromGregorian 2024 5 28,
                                                       memorialDay 2023 == fromGregorian 2023 5 30,
                                                       memorialDay 2022 == fromGregorian 2022 5 29,
                                                       memorialDay 2021 == fromGregorian 2021 5 30,
                                                       memorialDay 2020 == fromGregorian 2020 5 28 ]
                             in do
                               assertBool "testMemorialDay" $ and memorialDayDays
                               assertBool "testnonMemorialDay" $ not $ or nonMemorialDayDays)

testLaborDay :: Test
testLaborDay = TestCase (let laborDayDays = [ laborDay 2024 == fromGregorian 2024 9 2,
                                              laborDay 2023 == fromGregorian 2023 9 4,
                                              laborDay 2022 == fromGregorian 2022 9 5,
                                              laborDay 2021 == fromGregorian 2021 9 6,
                                              laborDay 2020 == fromGregorian 2020 9 7 ]
                             nonLaborDayDays = [ laborDay 2024 == fromGregorian 2024 9 3,
                                                 laborDay 2023 == fromGregorian 2023 9 5,
                                                 laborDay 2022 == fromGregorian 2022 9 4,
                                                 laborDay 2021 == fromGregorian 2021 9 7,
                                                 laborDay 2020 == fromGregorian 2020 9 6 ]
                         in do
                            assertBool "testLaborDay" $ and laborDayDays
                            assertBool "testnonLaborDay" $ not $ or nonLaborDayDays)


testThanksgivingDay :: Test
testThanksgivingDay = TestCase (let thanksgivingDayDays = [ thanksgivingDay 2024 == fromGregorian 2024 11 28,
                                                            thanksgivingDay 2023 == fromGregorian 2023 11 23,
                                                            thanksgivingDay 2022 == fromGregorian 2022 11 24,
                                                            thanksgivingDay 2021 == fromGregorian 2021 11 25,
                                                            thanksgivingDay 2020 == fromGregorian 2020 11 26 ]
                                    nonThanksgivingDayDays = [ thanksgivingDay 2024 == fromGregorian 2024 11 29,
                                                               thanksgivingDay 2023 == fromGregorian 2023 11 24,
                                                               thanksgivingDay 2022 == fromGregorian 2022 11 20,
                                                               thanksgivingDay 2021 == fromGregorian 2021 11 26,
                                                               thanksgivingDay 2020 == fromGregorian 2020 11 25 ]
                                in do
                                   assertBool "testThanksgivingDay" $ and thanksgivingDayDays
                                   assertBool "testnonThanksgivingDay" $ not $ or nonThanksgivingDayDays)

main :: IO ()
main = do
  counts2 <- runTestTT $ TestList [ TestLabel "testMartinLutherDay" testMartinLutherDay,
                                    TestLabel "testPresidentDay" testPresidentDay,
                                    TestLabel "testMemorialDay" testMemorialDay,
                                    TestLabel "testLaborDay" testLaborDay,
                                    TestLabel "testThanksgivingDay" testThanksgivingDay ]
  if errors counts2 + failures counts2 == 0
    then exitSuccess
    else exitFailure
