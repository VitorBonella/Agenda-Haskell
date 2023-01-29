-- ############################ READING THE YEAR ############################


addSchedule _ _ [] = []
addSchedule month day ((it,dur):xs) = [(month,day,it,dur)] ++ addSchedule month day xs

readDay _ _ [] = []
readDay month day (x:xs) = (addSchedule month day (read x :: [(Int,Int)])) ++ (readDays month xs)

readDays _ [] = [] 
readDays month (x:xs) = readDay month (read x :: Int) xs

readMonth (x:xs) = readDays (read x :: Int) xs

readAgenda [] = []
readAgenda (x:xs) = (readMonth x) ++ (readAgenda xs) 


readMonths [] = []
readMonths lines = [takeWhile (/="") lines] ++ (readMonths (dropWhile (=="") (dropWhile (/="") lines)))
        
    
readCalendar = do

    b <- doesFileExist "agenda.txt"

    if b then do
        content <- readFile "agenda.txt"
        
        return (readAgenda (readMonths (lines (content))))
    else do
        writeFile "agenda.txt" ""
        initAgenda



-- ############################ WRITE AGENDA ############################

takeOnlyTheDay agendaData day = [x | x <- agendaData, (getD x) == day]

divedeInDays agendaData 32 = []
divedeInDays monthData day = [takeOnlyTheDay monthData day] ++ (divedeInDays monthData (day+1))

takeOnlyTheMonth agendaData month = [x | x <- agendaData, (getM x) == month]

divedeInMonths agendaData 13 = []
divedeInMonths agendaData month = [divedeInDays (takeOnlyTheMonth agendaData month) 1] ++ (divedeInMonths agendaData (month+1))


dayString day= (show [(it,du) | (_,_,it,du) <- day])

makeAgendaStringMonth month m = [show m] ++ concat [[show i,dayString d]| (i,d) <- zip [1..] month, not (null d)] ++ ["\n"]

makeAgendaString agendaData =  [makeAgendaStringMonth m i | (i,m) <- zip [1..] agendaData]

onlyMoreThanTwoElements dados = [d | d <- dados, (length d) > 3]

writeCalendar agendaData yearDaysOff = do
    
    -- putStrLn (show (makeAgendaString (divedeInMonths agendaData 1)))

    writeFile "agenda.txt" $ unlines (concat (onlyMoreThanTwoElements (makeAgendaString (divedeInMonths agendaData 1))))
    
    agenda yearDaysOff agendaData
