module Xtdo_Test where
import Xtdo
import Test.HUnit
import Data.Time.Calendar

-- NEXT: QuickCheck + Tasks probably can do something cool

d = fromGregorian
noData = ProgramData{recurring=[],tasks=[]}

xtdoTaskTests =
  [ "l only shows Today" ~:
    (testTasks, PrettyFormatter [Today]) ~=?
      (run ["l"] testTasks)

  , "l a shows all testTasks" ~:
    (testTasks, PrettyFormatter [Today, Next, Scheduled]) ~=?
      (run ["l", "a"] testTasks)

  , "l c shows all testTasks" ~:
    (testTasks, CompletionFormatter [Today, Next, Scheduled]) ~=?
      (run ["l", "c"] testTasks)

  , "d shows only today and next" ~:
    PrettyFormatter [Today, Next] ~=?
      (extractCategories $ run ["d", "my task"] testTasks)

  , "d removes any task that matches the given name" ~:
    [chaff] ~=? (extractTasks $ run ["d", "my task"] testTasks)

  , "d removes any task that matches the given name with hyphens" ~:
    [chaff] ~=? (extractTasks $ run ["d", "my-task"] testTasks)

  , "a adds a new unscheduled task" ~:
    [blankTask{name="newtask", scheduled=Nothing, category=Next}] ~=?
      (extractTasks $ run ["a", "newtask"] noData)

  , "a adds a new scheduled task for today" ~:
    [blankTask{name="newtask", scheduled=Just today, category=Today}] ~=?
      (extractTasks $ run ["a", "0", "newtask"] noData)

  , "a adds a new scheduled task for tomorrow" ~:
    [blankTask{name="newtask", scheduled=Just tomorrow, category=Scheduled}] ~=?
      (extractTasks $ run ["a", "1", "newtask"] noData)
  ]
  where testTasks = noData{tasks=
          [ blankTask{name="my task"}
          , chaff
          ]}
        run args programData = xtdo args programData today
        chaff = blankTask{name="chaff"}
        today = (d 2011 1 1)
        tomorrow = (d 2011 1 2)
        extractTasks (x, y) = tasks x
        extractCategories (x, y) = y

xtdoRecurTests =
  [ "l shows all recurring" ~:
    (testData, RecurringFormatter) ~=?
      (run ["l"] testData)
  , "a adds a new daily one" ~:
    [RecurringTaskDefinition{
      templateName="newtask",
      frequency=(RecurFrequency Day 1 0),
      nextOccurrence=tomorrow}] ~=?
      (extractRecurring $ run ["a", "1d", "newtask"] noData)
  ]
  where run args programData = xtdo (["r"] ++ args) programData today
        testData = noData
        today = (d 2011 1 1)
        tomorrow = (d 2011 1 2)
        extractRecurring (x, y) = recurring x

parseFrequencyTests =
  [ t "1d"     (RecurFrequency Day 1 0)
  , t "2d"     (RecurFrequency Day 2 0)
  , t "1w,sun" (RecurFrequency Week 1 0)
  , t "1w,mon" (RecurFrequency Week 1 1)
  , t "2w,sat" (RecurFrequency Week 2 6)
  ]
  where t str expected = "parseFrequencyTests parses " ++ str ~:
                          expected ~=? (parseFrequency str)

calculateNextOccurrenceTests =
  [ t (d 2011 2 1) (RecurFrequency Day 1 0) (d 2011 2 2)
  ]
  where t today frequency expected =
          "calculateNextOccurrence for " ++ (show frequency) ~:
          expected ~=? (calculateNextOccurrence today frequency)

createRecurringTests =
  [ "adds a recurring task next occuring today" ~:
    expected ~=? (tasks $ run programData)
  , "does not add duplicates" ~:
    expected ~=? (tasks $ run (run programData))
  , "recaluates nextOccurrence" ~:
    expectedRecurring ~=? (recurring $ run programData)
  ]
  where run x = createRecurring today x
        definition = RecurringTaskDefinition {
          templateName   = "newtask",
          frequency      = frequency,
          nextOccurrence = today
        }
        expected = [blankTask {
          name      = "newtask",
          scheduled = Just today,
          category  = Today
        }]
        expectedRecurring = [definition {nextOccurrence = tomorrow}]
        programData = noData{recurring = [definition]}
        frequency   = RecurFrequency Day 1 0
        today       = (d 2011 2 1)
        tomorrow    = (d 2011 2 2)

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

main = runTestTT $ TestList ( xtdoTaskTests ++
                              xtdoRecurTests ++
                              parseFrequencyTests ++
                              calculateNextOccurrenceTests ++
                              createRecurringTests ++
                              dayTests )
