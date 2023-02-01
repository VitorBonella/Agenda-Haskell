import ScheduleBT
import Schedule
import Menu
import Calendar
import AgendaFiles


main = do
    
    yearDaysOff <- readYear
    --putStrLn (show yearDaysOff)
    

    agendaData <- readCalendar
    -- agendaDataValid <- initAgenda
    -- putStrLn (show agendaData)
    
    menu agendaData
