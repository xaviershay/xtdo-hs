module Xtdo_Test where
import Xtdo
import Test.HUnit
import Data.Time.Calendar

-- NEXT: Add tests for "a", probably should move addCategory inside xtdo function
-- NEXT: QuickCheck + Tasks probably can do something cool

d = fromGregorian

xtdoTests =
  [ "l only shows Today" ~: 
    (tasks, [Today]) ~=? (xtdo ["l"] tasks today)
  , "l a shows all tasks" ~:
    (tasks, [Today, Next, Scheduled]) ~=? (xtdo ["l", "a"] tasks today)
  , "d shows only today and next" ~:
    [Today, Next] ~=? (snd $ xtdo ["d", "mytask"] tasks today)
  , "d removes any task that matches the given name" ~:
    [chaff] ~=? (fst $ xtdo["d", "mytask"] tasks today)
  ]
  where tasks = 
          [ Task{name="mytask", scheduled=Nothing, category=Next}
          , chaff
          ]
        chaff = Task{name="chaff",  scheduled=Nothing, category=Next}
        today = (d 2011 1 1)
dayTests =
  [ t "1d"  (d 2011 2 2)
  , t "2d"  (d 2011 2 3)
  , t "20d" (d 2011 2 21)
  , t "1w"  (d 2011 2 8)
  , t "1m"  (d 2011 3 1)
  , t "1y"  (d 2012 2 1)
  ]
  where t str expected = "Xtdo.day parses " ++ str ~: expected ~=? (day (d 2011 2 1) str)

main = runTestTT $ TestList ( xtdoTests ++ dayTests )
