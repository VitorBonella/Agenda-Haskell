import Menu

-- https://gist.github.com/bfb/3855780

type Hour = Int
type Time = Int

type Scheduling = (Hour,Time)

type Day = (Int, [Scheduling])

type Month = (Bool, Int, [Day])
type Agenda = [Month]

type DayOff = [Bool]

type Year = (Bool, [DayOff])

agenda yearDaysOff agendaData = do
    
    showMenu
    op <- leOp

    if op == 0 then
        putStrLn "Saindo da Agenda"
    else
        agenda yearDaysOff agendaData

main = do
    yearDaysOff <- readYear
    -- putStrLn (show year)

    agendaData <- initAgenda
    putStrLn (show agendaData)

    agenda yearDaysOff agendaData





getHour (h , _) = h
getTime (_ , t) = t

-- Verify wheter the requested time is free
verifyAvailableSchedule ((h,t):ss) schedule
    | h == (getHour schedule) = False
    | null ss = True
    | otherwise = verifyAvailableSchedule ss schedule

-- Verify wheter the day is util and find the requested day
verifyAvailableDay ((d,ss):ds) schedule day
    | d == day = verifyAvailableSchedule ss schedule
    | null ds = False
    | otherwise = verifyAvailableDay ds schedule day

-- Verify whether the schedule time is available, this funcition search first for the month in agenda
-- AvailableTime :: Agenda -> Year -> Day -> Month -> Scheduling -> Bool
availableTime ((b,m,ds):agenda) yearInfo month day schedule 
    | m == month = verifyAvailableDay ds schedule day
    | null agenda = False
    | otherwise = availableTime agenda yearInfo month day schedule


-- ############################## AGENDA HANDLERS ##############################

--initDayList = [ (x, []) | x <- [1..31] ]

initAgenda = return [(False,i,[] :: Day) | i <- [1..12]]

-- ############################## INSERT A SCHEDULE ##############################



-- ############################## READING THE YEAR DAYS OFF ##############################

-- map read $ words string :: [Int] faz "1 2 3" virar [1,2,3]
stringToList string = map read $ words string :: [Int]

-- Test whether the day is in the list of days off
testPosition d (x:ds)
    | d == x = False
    | (null ds) = True
    | otherwise = testPosition d ds 

convertToDayOff string = [ testPosition d (stringToList string) | d <- [1..31]]

-- True = Util Day , False = Holiday or weekend
readDayOffs ds = [convertToDayOff x| x <- ds]

divideYearInput (x:xs) = (x,readDayOffs xs)

-- Read Year
readYear = do 
    content <- readFile "calendario.txt"
    --putStrLn (show (lines (content)))
    return (divideYearInput (lines (content)))   
