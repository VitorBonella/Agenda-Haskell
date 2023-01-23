import ScheduleBT
import Schedule
import Menu


main = do
    
    -- yearDaysOff <- readYear
    -- putStrLn (show yearDaysOff)

    agendaData <- return emptyScheduleTree
    -- agendaDataValid <- initAgenda
    -- putStrLn (show agendaData)
    
    menu agendaData
