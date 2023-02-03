import ScheduleBT
import Schedule
import Menu
import Calendar
import AgendaFiles

main = do
    
    yearDaysOff <- readYear -- ([Char],[[Bool]])
    --putStrLn (show yearDaysOff)

    agendaData <- readCalendar yearDaysOff
    -- agendaDataValid <- initAgenda
    -- putStrLn (show agendaData)
    
    menu yearDaysOff agendaData 
