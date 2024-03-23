module Main (main) where

import System.Exit (exitFailure, exitSuccess)
import Test.HUnit

import Data.Time.Calendar (fromGregorian)
import QuantLib.Time.Calendars.UnitedStates (martinLutherDay, presidentsDay)

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
                                   assertBool "testPresidentDay" $ not $ or nonPresidentDayDays)


main :: IO ()
main = do
  counts2 <- runTestTT $ TestList [ TestLabel "testMartinLutherDay" testMartinLutherDay,
                                    TestLabel "testPresidentDay" testPresidentDay ]
  if errors counts2 + failures counts2 == 0
    then exitSuccess
    else exitFailure
