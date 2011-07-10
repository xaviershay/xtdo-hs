import Xtdo

import System.Environment
import Data.Time.Clock

import Data.List

main :: IO ()
main = do
  args        <- getArgs
  programData <- loadData
  now         <- getCurrentTime
  let today    = (utctDay now)
  let result = xtdo args (decoratedData programData today) today
  format result
  saveData $ fst result
  where decoratedData programData today =
          ProgramData{
            tasks     = addCategory (tasks programData) today,
            recurring = (recurring programData)
          }

loadData :: IO ProgramData
loadData = do
  environment <- getEnvironment
  let path =  envPathWithDefault environment
  loadYaml path

saveData programData = do
  environment <- getEnvironment
  let path =  envPathWithDefault environment
  saveYaml path programData

envPathWithDefault = dataPath . find ((==) "XTDOHS_PATH" . fst)

dataPath :: Maybe (String, String) -> String
dataPath path = dataPath' path
dataPath' Nothing     = "tasks.yml"
dataPath' (Just path) = snd path
