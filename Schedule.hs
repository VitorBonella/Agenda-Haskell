module Schedule where

data Schedule = Schedule {day :: Int, 
                         month :: Int, 
                         initialTime :: Int,
                         duration :: Int, 
                         scheduleType :: ScheduleType, 
                         description :: String }

data ScheduleType = Remote | OnPlace

instance Show ScheduleType where
    show Remote = "Remote"
    show OnPlace = "OnPlace"

instance Eq Schedule where
    (==) schedule1 schedule2 = (day schedule1 == day schedule2) && (month schedule1 == month schedule2) && (initialTime schedule1 == initialTime schedule2)

instance Ord Schedule where
    compare schedule1 schedule2 = compare (day schedule1, month schedule1, initialTime schedule1) (day schedule2, month schedule2, initialTime schedule2)

instance Show Schedule where
    show schedule = (show $ scheduleType schedule) ++ "\n" ++ (show $ initialTime schedule) ++ ", " ++ (show $ duration schedule) ++ "\n" ++ (description schedule)