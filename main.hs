import ScheduleBT
import Schedule
import Menu
import Calendar
import AgendaFiles


main = do
    
    yearDaysOff <- readYear
    --putStrLn (show yearDaysOff)
    
    readCalendar

    agendaData <- return emptyScheduleTree
    -- agendaDataValid <- initAgenda
    -- putStrLn (show agendaData)
    
    menu agendaData
