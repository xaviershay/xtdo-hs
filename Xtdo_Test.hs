module Xtdo_Test where
import Xtdo
import Test.HUnit
import Data.Time.Calendar

-- NEXT: QuickCheck + Tasks probably can do something cool

d = fromGregorian

xtdoTests =
  [ "l only shows Today" ~:
    (tasks, [Today], PrettyFormatter) ~=? (run ["l"] tasks)

  , "l a shows all tasks" ~:
    (tasks, [Today, Next, Scheduled], PrettyFormatter) ~=? (run ["l", "a"] tasks)

  , "l c shows all tasks" ~:
    (tasks, [Today, Next, Scheduled], CompletionFormatter) ~=? (run ["l", "c"] tasks)

  , "d shows only today and next" ~:
    [Today, Next] ~=? (extractCategories $ run ["d", "my task"] tasks)

  , "d removes any task that matches the given name" ~:
    [chaff] ~=? (extractTasks $ run ["d", "my task"] tasks)

  , "d removes any task that matches the given name with hyphens" ~:
    [chaff] ~=? (extractTasks $ run ["d", "my-task"] tasks)

  , "a adds a new unscheduled task" ~:
    [blankTask{name="newtask", scheduled=Nothing, category=Next}] ~=?
      (extractTasks $ run ["a", "newtask"] [])

  , "a adds a new scheduled task for today" ~:
    [blankTask{name="newtask", scheduled=Just today, category=Today}] ~=?
      (extractTasks $ run ["a", "0", "newtask"] [])

  , "a adds a new scheduled task for tomorrow" ~:
    [blankTask{name="newtask", scheduled=Just tomorrow, category=Scheduled}] ~=?
      (extractTasks $ run ["a", "1", "newtask"] [])
  ]
  where tasks =
          [ blankTask{name="my task"}
          , chaff
          ]
        run args tasks = xtdo args ProgramData{tasks=tasks, recurring=[]} today
        chaff = blankTask{name="chaff"}
        today = (d 2011 1 1)
        tomorrow = (d 2011 1 2)
        extractTasks (x, y, z) = x
        extractCategories (x, y, z) = y
        extractFormatter (x, y, z) = z

dayTests =
  [ t "1d"  (d 2011 2 1)  (d 2011 2 2)
  , t "2d"  (d 2011 2 1)  (d 2011 2 3)
  , t "20d" (d 2011 2 1)  (d 2011 2 21)
  , t "28d" (d 2011 2 1)  (d 2011 3 1)
  , t "1w"  (d 2011 2 1)  (d 2011 2 8)
  , t "1m"  (d 2011 2 1)  (d 2011 3 1)
  , t "1y"  (d 2011 2 1)  (d 2012 2 1)
  , t "0"   (d 2011 2 1)  (d 2011 2 1)
  , t "1"   (d 2011 2 1)  (d 2011 2 2)
  , t "1m"  (d 2011 12 1) (d 2012 1 1)
  , t "1m"  (d 2011 1 31) (d 2011 2 28)
  , t "1y"  (d 2004 2 29) (d 2005 2 28)
  ]
  where t str from expected = "Xtdo.day parses " ++ str ~:
                                expected ~=? (day from str)

main = runTestTT $ TestList ( xtdoTests ++ dayTests )
