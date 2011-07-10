import Xtdo

import System.Environment
import Data.Time.Clock

main :: IO ()
main = do
  args        <- getArgs
  programData <- loadYaml
  now         <- getCurrentTime
  let today = (utctDay now)
  finish $ xtdo args (decoratedData programData today) today
  where decoratedData programData today =
          ProgramData{
            tasks     = addCategory (tasks programData) today,
            recurring = (recurring programData)
          }
