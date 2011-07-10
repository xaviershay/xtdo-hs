import Xtdo

import System.Environment
import Data.Time.Clock

main :: IO ()
main = do
  args <- getArgs
  programData <- loadYaml
  now <- getCurrentTime
  let today = (utctDay now)
  finish $ xtdo args (decoratedData programData today) today
 -- ProgramData{tasks=tasks,recurring=recurring} =
  where decoratedData programData today =
          ProgramData{tasks=decoratedTasks,recurring=(recurring programData)}
          where decoratedTasks = addCategory (tasks programData) today
