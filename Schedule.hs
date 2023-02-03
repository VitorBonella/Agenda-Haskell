module Schedule where

data Schedule = Schedule {day :: Int, 
                         month :: Int, 
                         initialTime :: Int,
                         duration :: Int, 
                         scheduleType :: ScheduleType, 
                         description :: String }

data ScheduleType = Remote | OnPlace

instance Show ScheduleType where
    show Remote = "videoconferencia"
    show OnPlace = "presencial"

instance Eq Schedule where
    (==) schedule1 schedule2 = (day schedule1 == day schedule2) && (month schedule1 == month schedule2) && (initialTime schedule1 == initialTime schedule2)

instance Ord Schedule where
    compare schedule1 schedule2 = compare (month schedule1, day schedule1, initialTime schedule1) ( month schedule2, day schedule2, initialTime schedule2)

instance Show Schedule where
    show schedule = (show $ scheduleType schedule) ++ "\n" ++ (show $ initialTime schedule) ++ "," ++ (show $ duration schedule) ++ "\n" ++ (description schedule)

addDayReset:: Schedule -> Schedule
addDayReset s = (Schedule ((day s)+1) (month s) 8 (duration s) (scheduleType s) (description s))

addAnHour:: Schedule -> Schedule
addAnHour s = (Schedule (day s) (month s) ((initialTime s)+1) (duration s) (scheduleType s) (description s))

addMonthReset:: Schedule -> Schedule
addMonthReset s = (Schedule 1 ((month s)+1) 8 (duration s) (scheduleType s) (description s))