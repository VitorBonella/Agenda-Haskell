module ScheduleIO where

import Schedule
import ScheduleBT

readSchedule :: IO Schedule
readSchedule = do
    putStrLn "Enter month: "
    month <- readLn :: IO Int
    putStrLn "Enter day: "
    day <- readLn :: IO Int
    putStrLn "Enter initial time (hours): "
    hour <- readLn :: IO Int
    let time = hour
    putStrLn "Enter duration: "
    duration <- readLn :: IO Int
    putStrLn "Enter schedule type (Remote/OnPlace): "
    scheduleType <- getLine
    let sType = if scheduleType == "Remote" then Remote else OnPlace

    if scheduleType == "Remote" then do
        putStrLn "Enter the meeting plataform: "
        description <- getLine
        putStrLn "Enter Link: "
        link <- getLine
        return (Schedule day month time duration sType (description++"\n"++link))
    else do
        putStrLn "Enter description: "
        description <- getLine
        return (Schedule day month time duration sType description)
    

deleteSchedule :: ScheduleTree -> IO ScheduleTree
deleteSchedule tree = do
    putStrLn "Enter month: "
    month <- readLn :: IO Int
    putStrLn "Enter day: "
    day <- readLn :: IO Int
    putStrLn "Enter initial time (hours): "
    hour <- readLn :: IO Int
    let time = hour
    let newTree = delete day month time tree

    return newTree

reschedule :: ([Char],[[Bool]]) -> ScheduleTree -> IO ScheduleTree
reschedule yearDaysOff tree = do
    putStrLn "Enter the day of the schedule you want to reschedule: "
    day <- readLn :: IO Int
    putStrLn "Enter the month of the schedule you want to reschedule: "
    month <- readLn :: IO Int
    putStrLn "Enter the initial time (hours) of the schedule you want to reschedule: "
    hour <- readLn :: IO Int
    let time = hour
    putStrLn "Enter the new day: "
    newDay <- readLn :: IO Int
    putStrLn "Enter the new month: "
    newMonth <- readLn :: IO Int
    putStrLn "Enter the new initial time (hours): "
    newHour <- readLn :: IO Int
    putStrLn "Enter the new duration: "
    newDuration <- readLn :: IO Int
    let newTime = newHour

    case (searchSchedule tree month day time) of
        Just schedule -> do
            -- update or delete the schedule
            let newSchedule = Schedule newDay newMonth newTime newDuration (scheduleType schedule) (description schedule)
            let treeWithoutOldSchedule = delete day month time tree
            let newTree = insert yearDaysOff newSchedule treeWithoutOldSchedule
            return newTree
        Nothing -> return tree
    
    
checkAvailability :: ([Char],[[Bool]]) -> ScheduleTree -> IO ()
checkAvailability yearDaysOff tree = do
    putStrLn "Enter month: "
    month <- readLn :: IO Int
    putStrLn "Enter day: "
    day <- readLn :: IO Int
    putStrLn "Enter initial time (hours): "
    hour <- readLn :: IO Int
    let time = hour
    putStrLn "Enter duration: "
    duration <- readLn :: IO Int

    let valid = verify yearDaysOff (Schedule day month time duration Remote "") tree
    if valid then
        putStrLn "Valid Schedule"
    else
        putStrLn "Invalid Schedule"

readInsertMinSchedule:: ([Char],[[Bool]]) -> ScheduleTree -> IO ScheduleTree
readInsertMinSchedule yearDaysOff tree = do
    putStrLn "Enter month: "
    month <- readLn :: IO Int
    putStrLn "Enter day: "
    day <- readLn :: IO Int
    putStrLn "Enter duration: "
    duration <- readLn :: IO Int
    putStrLn "Enter schedule type (Remote/OnPlace): "
    scheduleType <- getLine
    let sType = if scheduleType == "Remote" then Remote else OnPlace
    
    if scheduleType == "Remote" then do

        putStrLn "Enter the meeting plataform: "
        description <- getLine
        putStrLn "Enter Link: "
        link <- getLine

        case (returnMinSchedule yearDaysOff tree (Schedule day month 8 duration sType (description++"\n"++link))) of
            Just schedule -> do
                putStrLn (show schedule)
                return (insert yearDaysOff schedule tree)
            Nothing -> do
                putStrLn "acho nada"
                return tree


    else do

        putStrLn "Enter description: "
        description <- getLine

        case (returnMinSchedule yearDaysOff tree (Schedule day month 8 duration sType description)) of
            Just schedule -> do
                putStrLn (show schedule)
                return (insert yearDaysOff schedule tree)
            Nothing -> do
                putStrLn "acho nada"
                return tree



    

