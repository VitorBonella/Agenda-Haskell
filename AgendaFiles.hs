module AgendaFiles where

import ScheduleBT
import System.Directory
import Schedule

-- ############################ READING THE YEAR ############################

getRest:: [String] -> String -> [String]
getRest content scheduleType --deixa o mes e o dia e dropa o compromisso 
    | scheduleType == "presencial" = [content!!0] ++ [content!!1] ++ drop 5 content 
    | scheduleType == "videoconferencia" = [content!!0] ++ [content!!1] ++ drop 6 content
    | otherwise =  [content!!0] ++ [content!!2] ++ drop 3 content

getDay:: [String] -> String -> [String]
getDay content scheduleType  -- pega o conteudinho
    | scheduleType == "presencial" = take 5 content
    | scheduleType == "videoconferencia" = take 6 content
    | otherwise = []

schedulesRead:: [String] -> [[String]]
schedulesRead [] = []
schedulesRead content
    | length(content) < 5 = [] -- nao tiver mais dias
    | otherwise = [getDay content (content!!2)] ++ schedulesRead (getRest content (content!!2))


readDays:: [[String]] -> [[String]]
readDays [] = [] 
readDays (x:xs) = schedulesRead x ++ readDays xs

readMonths:: [String] -> [[String]]
readMonths [] = []
readMonths lines = [takeWhile (/="") lines] ++ (readMonths (dropWhile (=="") (dropWhile (/="") lines)))


strToInitialTimeAndDuration:: String -> (Int,Int)
strToInitialTimeAndDuration str
    | length(str) == 3 = strToInitialTimeAndDurationMorning str
    | otherwise = strToInitialTimeAndDurationAfternoon str


strToInitialTimeAndDurationMorning:: String -> (Int,Int)
strToInitialTimeAndDurationMorning str = ( (read first :: Int) , (read second :: Int) )
    where
        first = take 1 str
        second = take 1 (drop 2 str)

strToInitialTimeAndDurationAfternoon:: String -> (Int,Int)
strToInitialTimeAndDurationAfternoon str = ( (read first :: Int) , (read second :: Int) )
    where
        first = take 2 str
        second = take 1 (drop 3 str)

stringToSchedule:: [String] -> Schedule
stringToSchedule strs = Schedule day month time duration sType description
    where
        day = (read (strs!!1) :: Int)
        month = (read (strs!!0) :: Int)
        (time, duration) = strToInitialTimeAndDuration (strs!!3)
        sType = case strs!!2 of
            "presencial" -> OnPlace
            "videoconferencia" -> Remote
        description = case strs!!2 of
            "presencial" -> strs!!4
            "videoconferencia" -> strs!!4 ++ "\n" ++ strs!!5

stringListToScheduleList:: [[String]] -> [Schedule]
stringListToScheduleList strings = [stringToSchedule strs | strs <- strings, length(strs) > 0]

scheduleListToScheduleBt:: ([Char],[[Bool]]) -> [Schedule] -> ScheduleTree -> ScheduleTree
scheduleListToScheduleBt yearDaysOff [] tree = tree 
scheduleListToScheduleBt yearDaysOff (x:xs) tree = scheduleListToScheduleBt yearDaysOff xs (insert yearDaysOff x tree)

readCalendar:: ([Char],[[Bool]]) -> IO(ScheduleTree)
readCalendar yearDaysOff = do

    b <- doesFileExist "agenda.txt"

    if b then do
        content <- readFile "agenda.txt"
        
        let months = readMonths (lines content)
        -- putStrLn (show months)
        let scheduleListStr = readDays months
        -- putStrLn (show scheduleListStr)
        let scheduleList = stringListToScheduleList scheduleListStr
        -- putStrLn (show scheduleList)
        let bt = scheduleListToScheduleBt yearDaysOff scheduleList emptyScheduleTree

        return bt

    else do
        writeFile "agenda.txt" ""
        return emptyScheduleTree



-- ############################ WRITE AGENDA ############################

takeOnlyTheDay:: [Schedule] -> Int -> [Schedule]
takeOnlyTheDay monthList dayy = [x | x <- monthList, (day x) == dayy]

divedeInDays:: [Schedule] -> Int -> [[Schedule]]
divedeInDays monthList 32 = []
divedeInDays monthList day = [takeOnlyTheDay monthList day] ++ (divedeInDays monthList (day+1))

takeOnlyTheMonth:: [Schedule] -> Int -> [Schedule]
takeOnlyTheMonth scheduleList monthh = [x | x <- scheduleList, (month x) == monthh]

divedeInMonths:: [Schedule] -> Int -> [[[Schedule]]]
divedeInMonths scheduleList 13 = []
divedeInMonths scheduleList month = [divedeInDays (takeOnlyTheMonth scheduleList month) 1] ++ (divedeInMonths scheduleList (month+1))

dayString:: [Schedule] -> String
dayString day = (concat ([show dd ++"\n" | dd <- (init day)] ++ [show (last day)])) 

makeAgendaStringMonth:: [[Schedule]] -> Int -> [String]
makeAgendaStringMonth month m = [show m] ++ concat [[show i,dayString d]| (i,d) <- zip [1..] month, not (null d)] ++ [""]

makeAgendaString:: [[[Schedule]]] -> [[String]]
makeAgendaString scheduleList =  [makeAgendaStringMonth m i| (i,m) <- zip [1..] scheduleList]

onlyMoreThanTwoElements:: [[String]] -> [[String]]
onlyMoreThanTwoElements dados = [d | d <- dados, (length d) > 3]

writeCalendar:: ScheduleTree -> IO()
writeCalendar tree = do
    
    
    let sch_list = scheduleBtToScheduleList tree
    -- putStrLn (show ((makeAgendaString (divedeInMonths sch_list 1))))

    writeFile "agenda.txt" $ unlines (concat (onlyMoreThanTwoElements (makeAgendaString (divedeInMonths sch_list 1))))

