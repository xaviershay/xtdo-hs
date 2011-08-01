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

  , "a adds a new unscheduled task" ~:
    [blankTask{name="this is not a 3d", scheduled=Nothing, category=Next}] ~=?
      (extractTasks $ run ["a", "this is not a 3d"] noData)
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
        run args programData     = xtdo args programData today
        chaff                    = blankTask{name="chaff"}
        today                    = (d 2011 1 1)
        tomorrow                 = (d 2011 1 2)

extractTasks (x, y)      = tasks x
extractCategories (x, y) = y

addCategoryTests =
  [ "adds Today to tasks that where scheduled for yesterday" ~:
    [blankTask{ scheduled = Just yesterday, category = Today }] ~=?
    addCategory [blankTask{ scheduled = Just yesterday }] today
  ]
  where today = (d 2011 1 2)
        yesterday = (d 2011 1 1)

xtdoBumpTests =
  [ "b 0 bumps to today" ~:
    expectedTasks ~=? (extractTasks $ run ["b", "0", "newtask"] testTasks)
  , "b 1 bumps to tomorrow" ~:
    expectedTomorrow ~=? (extractTasks $ run ["b", "1", "newtask"] testTasks)
  ]
  where testTasks = noData{tasks=
          [ blankTask{name="newtask"}
          ]}
        expectedTasks =
          [ blankTask{
              name      = "newtask",
              category  = Today,
              scheduled = Just today
            }
          ]
        expectedTomorrow =
          [ blankTask{
              name      = "newtask",
              category  = Scheduled,
              scheduled = Just tomorrow
            }
          ]
        run args x = xtdo args x today
        today      = (d 2011 1 1)
        tomorrow   = (d 2011 1 2)

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
  , "d removes a task" ~:
    [] ~=? (extractRecurring $ run ["d", "existing", "task"] testData)
  ]
  where run args programData = xtdo (["r"] ++ args) programData today
        testData = noData{
          recurring = [RecurringTaskDefinition{
            templateName   = "existing task",
            frequency      = (RecurFrequency Day 1 0),
            nextOccurrence = tomorrow
          }]
        }
        today = (d 2011 1 1)
        tomorrow = (d 2011 1 2)
        extractRecurring (x, y) = recurring x

recurTestData t =
  [ t "1d"     (RecurFrequency Day 1 0)
  , t "2d"     (RecurFrequency Day 2 0)
  , t "1w,sun" (RecurFrequency Week 1 0)
  , t "1w,mon" (RecurFrequency Week 1 1)
  , t "2w,sat" (RecurFrequency Week 2 6)
  , t "1m,1"   (RecurFrequency Month 1 0)
  , t "2m,10"  (RecurFrequency Month 2 9)
  , t "1y,1"   (RecurFrequency Year 1 0)
  , t "2y,10"  (RecurFrequency Year 2 9)
  ]

parseFrequencyTests = recurTestData t
  where t str expected = "parseFrequencyTests parses " ++ str ~:
                          expected ~=? (parseFrequency str)

frequencyToStringTests = recurTestData t
  where t expected freq = "frequenccyToString formats " ++ expected ~:
                          expected ~=? (frequencyToString freq)

calculateNextOccurrenceTests =
  [ t (d 2011 2 1) (RecurFrequency Day 1 0) (d 2011 2 2)
  , t (d 2011 2 1) (RecurFrequency Day 2 0) (d 2011 2 3)
  , t (d 2011 7 10) (RecurFrequency Week 1 0) (d 2011 7 17)
  , t (d 2011 7 11) (RecurFrequency Week 1 0) (d 2011 7 17)
  , t (d 2011 7 16) (RecurFrequency Week 1 0) (d 2011 7 17)
  , t (d 2011 7 11) (RecurFrequency Week 2 0) (d 2011 7 24)
  , t (d 2011 7 11) (RecurFrequency Week 2 1) (d 2011 7 25)
  , t (d 2011 7 11) (RecurFrequency Week 2 2) (d 2011 7 12)
  , t (d 2011 7 10) (RecurFrequency Week 1 1) (d 2011 7 11)
  , t (d 2011 7 1) (RecurFrequency Month 1 0) (d 2011 8 1)
  , t (d 2011 7 2) (RecurFrequency Month 1 0) (d 2011 8 1)
  , t (d 2011 7 1) (RecurFrequency Month 1 1) (d 2011 7 2)
  , t (d 2011 1 2) (RecurFrequency Year  1 0) (d 2012 1 1)
  , t (d 2011 1 2) (RecurFrequency Year  1 0) (d 2012 1 1)
  , t (d 2011 1 1) (RecurFrequency Year  1 1) (d 2011 1 2)
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
                              xtdoBumpTests ++
                              addCategoryTests ++
                              parseFrequencyTests ++
                              frequencyToStringTests ++
                              calculateNextOccurrenceTests ++
                              createRecurringTests ++
                              dayTests )
