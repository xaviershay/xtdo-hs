module Xtdo_Test where
import Xtdo
import Test.HUnit
import Data.Time.Calendar

dayTests =
  [ t "1d"  (d 2011 2 2)
  , t "2d"  (d 2011 2 3)
  , t "20d" (d 2011 2 21)
  , t "1w"  (d 2011 2 8)
  , t "1m"  (d 2011 3 1)
  , t "1y"  (d 2012 2 1)
  ]
  where t str expected = "Xtdo.day parses " ++ str ~: expected ~=? (day (d 2011 2 1) str)
        d = fromGregorian


main = runTestTT $ TestList dayTests
