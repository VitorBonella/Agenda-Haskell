import Menu

-- https://gist.github.com/bfb/3855780

type Hour = Int
type Time = Int

type Scheduling = (Hour,Time)

type Day = (Int, [Scheduling)])

type Month = Int
type Agenda = (Bool, Month, [Day])

type DayOff = (Month,Day)

type Year = (Bool, [DayOff])

getHour (h , _) = h
getTime (_ , t) = t

-- Verify wheter the requested time is free
verifyAvailableSchedule ((h,t):ss) schedule
    | h == (getHour schedule) = False
    | null ss = True
    | otherwise verifyAvailableSchedule ss schedule

-- Verify wheter the day is util and find the requested day
verifyAvailableDay ((d,ss):ds) schedule day
    | d == day = verifyAvailableSchedule ss schedule
    | null ds = False
    | otherwise = verifyAvailableDay ds schedule day

-- Verify whether the schedule time is available, this funcition search first for the month in agenda
AvailableTime :: Agenda -> Year -> Day -> Month -> Scheduling -> Bool
AvailableTime ((b,m,ds):agenda) yearInfo month day schedule 
    | m == month = verifyAvailable ds schedule day
    | null agenda = False
    | otherwise = AvailableTime agenda yearInfo month day schedule


main = do
    showMenu
    op <- leOp
    main