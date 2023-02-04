module ScheduleIO where

import Schedule
import ScheduleBT
import Control.Monad

readBasedOnType:: String -> IO String
readBasedOnType sType = do
    if (sType == "videoconferencia") then do
        putStrLn "Enter the meeting plataform: "
        description <- getLine
        putStrLn "Enter Link: "
        link <- getLine
        return (description++"\n"++link)
        --return (Schedule day month time duration sType (description++"\n"++link))
    else do
        putStrLn "Enter description: "
        description <- getLine
        return description

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
    putStrLn "Enter schedule type (videoconferencia/presencial): "
    scheduleType <- getLine
    let sType = if scheduleType == "videoconferencia" then Remote else OnPlace
    descriptionF <- readBasedOnType scheduleType

    return (Schedule day month time duration sType descriptionF)
    

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
    monthh <- readLn :: IO Int
    putStrLn "Enter day: "
    dayy <- readLn :: IO Int
    putStrLn "Enter duration: "
    duration <- readLn :: IO Int
    putStrLn "Enter schedule type (videoconferencia/presencial): "
    scheduleType <- getLine
    let sType = if scheduleType == "videoconferencia" then Remote else OnPlace
    descriptionF <- readBasedOnType scheduleType

    case (returnMinSchedule yearDaysOff tree (Schedule dayy monthh 8 duration sType descriptionF)) of
        Just schedule -> do
            putStrLn ("\n" ++ "Sucess Scheduled: " ++ (show (day schedule)) ++ "/" ++ (show (month schedule)) ++ "\n" ++(show schedule) ++ "\n")
            return (insert yearDaysOff schedule tree)
        Nothing -> do
            putStrLn "Not Sucessful"
            return tree
   


readInsertMinIntervalSchedule:: ([Char],[[Bool]]) -> ScheduleTree -> IO ScheduleTree
readInsertMinIntervalSchedule yearDaysOff tree = do
    
    putStrLn "Enter month: "
    monthh <- readLn :: IO Int
    putStrLn "Enter day: "
    dayy <- readLn :: IO Int
    putStrLn "Enter duration: "
    duration <- readLn :: IO Int
    putStrLn "Enter schedule type (videoconferencia/presencial): "
    scheduleType <- getLine
    let sType = if scheduleType == "videoconferencia" then Remote else OnPlace
    descriptionF <- readBasedOnType scheduleType
    putStrLn "Deadline in days:"
    max_days <- readLn :: IO Int

    overTheMonth <- verifyMonthDayAux  yearDaysOff monthh (dayy+max_days)
    let schedule_list =  scheduleBtToScheduleList tree

    if not (overTheMonth) then do
        putStrLn "Busca de intervalo ultrapassa um mês"
        return tree
    else do
        let selected_days = (selectMonthAndDays monthh dayy max_days schedule_list)
        let minInterval = returnMinIndex ((getTheMinimumIndex (allIntervals yearDaysOff selected_days) duration))
        
        case minInterval of
            Just interval -> do
                let previousSchedule = (selected_days !! (fst interval))

                let schedule = returnMinSchedule yearDaysOff tree (changeScheduleTypeAndDescAndDur previousSchedule sType descriptionF duration)
            
                case schedule of
                    Just schedule -> do
                        putStrLn ("\n" ++ "Sucess Scheduled: " ++ (show (day schedule)) ++ "/" ++ (show (month schedule)) ++ "\n" ++(show schedule) ++ "\n")
                        return (insert yearDaysOff schedule tree)
                    Nothing -> do
                        putStrLn "Not Sucessful"
                        return tree
            Nothing -> do
                return tree
            

    

readInsertMaxIntervalSchedule:: ([Char],[[Bool]]) -> ScheduleTree -> IO ScheduleTree
readInsertMaxIntervalSchedule yearDaysOff tree = do
    
    putStrLn "Enter month: "
    monthh <- readLn :: IO Int
    putStrLn "Enter day: "
    dayy <- readLn :: IO Int
    putStrLn "Enter duration: "
    duration <- readLn :: IO Int
    putStrLn "Enter schedule type (videoconferencia/presencial): "
    scheduleType <- getLine
    let sType = if scheduleType == "videoconferencia" then Remote else OnPlace
    descriptionF <- readBasedOnType scheduleType
    putStrLn "Deadline in days:"
    max_days <- readLn :: IO Int

    overTheMonth <- verifyMonthDayAux  yearDaysOff monthh (dayy+max_days)
    let schedule_list =  scheduleBtToScheduleList tree

    if not (overTheMonth) then do
        putStrLn "Busca de intervalo ultrapassa um mês"
        return tree
    else do
        let selected_days = (selectMonthAndDays monthh dayy max_days schedule_list)
        let minInterval = returnMaxIndex ((getTheMinimumIndex (allIntervals yearDaysOff selected_days) duration))
        
        case minInterval of
            Just interval -> do
                let previousSchedule = (selected_days !! (fst interval))

                let schedule = returnMinSchedule yearDaysOff tree (changeScheduleTypeAndDescAndDur previousSchedule sType descriptionF duration)
            
                case schedule of
                    Just schedule -> do
                        putStrLn ("\n" ++ "Sucess Scheduled: " ++ (show (day schedule)) ++ "/" ++ (show (month schedule)) ++ "\n" ++(show schedule) ++ "\n")
                        return (insert yearDaysOff schedule tree)
                    Nothing -> do
                        putStrLn "Not Sucessful"
                        return tree
            Nothing -> do
                return tree

