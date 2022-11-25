import Menu
import System.Directory

getM (x,_,_,_) = x
getD (_,x,_,_) = x
getIT (_,_,x,_) = x
getDU (_,_,_,x) = x


lesserEqual schedule1 schedule2
    | getM schedule1 < getM schedule2 = True
    | getM schedule1 == getM schedule2 && getD schedule1 < getD schedule2 = True
    | getM schedule1 == getM schedule2 && getD schedule1 == getD schedule2 && getIT schedule1 <= getIT schedule2 = True
    | otherwise = False

greater schedule1 schedule2
    | getM schedule1 > getM schedule2 = True
    | getM schedule1 == getM schedule2 && getD schedule1 > getD schedule2 = True
    | getM schedule1 == getM schedule2 && getD schedule1 == getD schedule2 && getIT schedule1 > getIT schedule2 = True
    | otherwise = False

quicksort [] = []  
quicksort (x:xs) =   
    let left = quicksort [a | a <- xs, lesserEqual a x]  
        right = quicksort [a | a <- xs,greater a x]  
    in  left ++ [x] ++ right  


trataOP op yearDaysOff agendaData
    | op == 1 =  main
    | op == 2 = verifySchedule yearDaysOff agendaData
    | op == 3 = readInsertSchedule yearDaysOff agendaData
    | op == 4 = readInsertMinSchedule yearDaysOff agendaData
    | op == 7 = readDeleteSchedule yearDaysOff agendaData
    | op == 8 = readReSchedule yearDaysOff agendaData 
    | op == 9 = writeCalendar agendaData yearDaysOff
    | otherwise = agenda yearDaysOff agendaData


agenda yearDaysOff agendaData = do

    putStrLn (show agendaData)

    showMenu
    op <- leOp

    if op == 0 then
        putStrLn "Saindo da Agenda"
    else
        trataOP op yearDaysOff (quicksort agendaData)


main = do
    
    yearDaysOff <- readYear
    -- putStrLn (show yearDaysOff)

    agendaData <- readCalendar
    -- agendaDataValid <- initAgenda
    -- putStrLn (show agendaData)
    
    agenda yearDaysOff agendaData



--insertOnlyValid
initAgenda = return ([] :: [(Int,Int,Int,Int)])


-- ############################ VERIFIERS ############################

getMonthDayList agendaData m d = [x | x <- agendaData, (getM x) == m && (getD x) == d]

testT it t
    | it < 12 && t >= 12 = t + 2
    | otherwise = t

getHourOccupied it du = [testT it t | t <- [it..(it+du-1)]]

createListOfOccupation agendaData m d = concat [getHourOccupied (getIT x) (getDU x)| x <- (getMonthDayList agendaData m d)]

verifyIT it du
    | it + du + 2 > 18 && it < 12 = False
    | it + du > 18 && it >=14 = False
    | it >= 8 && it < 12 || it >= 14 && it < 18 = True
    | otherwise = False


intersectList xs ys = [y | y <- ys, y `elem` xs]

-- verifyAlreadyScheduled agendaData m d it = it `elem` (createListOfOccupation agendaData m d)
verifyAlreadyScheduled agendaData m d it du = not (null (intersectList (getHourOccupied it du) (createListOfOccupation agendaData m d))) || it `elem` (createListOfOccupation agendaData m d)

dayOff yearDaysOff m d 
    | m <=12 && d <= 31 = ((snd yearDaysOff) !! (m-1)) !! (d-1)
    | otherwise = False

-- ############################ VERIFY SCHEDULE ############################

returnBoolSchedule yearDaysOff agendaData m d it du = return (verifyIT it du && not (verifyAlreadyScheduled agendaData m d it du) && (dayOff yearDaysOff m d))

verifySchedule yearDaysOff agendaData = do
    -- Month
    putStrLn "Mes:"
    m <- getLine
    -- Day 
    putStrLn "Dia:"
    d <- getLine
    -- Schedule Initial Time
    putStrLn "Horário de início:"
    it <- getLine
    -- Schedule Duration
    putStrLn "Duração:"
    dur <- getLine

    valid <- returnBoolSchedule yearDaysOff agendaData (read m :: Int) (read d :: Int) (read it :: Int) (read dur :: Int)

    if valid then
        putStrLn "Compromisso pode ser marcado"
    else
        putStrLn "Compromisso não pode ser marcado"

    agenda yearDaysOff agendaData

-- ############################ INSERT SCHEDULE ############################

insertScheduleAux yearDaysOff agendaData m d it du
    | verifyIT it du && not (verifyAlreadyScheduled agendaData m d it du) && (dayOff yearDaysOff m d) = agenda yearDaysOff (agendaData ++ [(m,d,it,du)])
    | otherwise = agenda yearDaysOff agendaData

readInsertSchedule yearDaysOff agendaData = do
    -- Month
    putStrLn "Mes:"
    m <- getLine
    -- Day 
    putStrLn "Dia:"
    d <- getLine
    -- Schedule Initial Time
    putStrLn "Horário de início:"
    it <- getLine
    -- Schedule Duration
    putStrLn "Duração:"
    dur <- getLine

    insertScheduleAux yearDaysOff agendaData (read m :: Int) (read d :: Int) (read it :: Int) (read dur :: Int)

-- ############################ RESCHEDULE ############################


reSchedule yearDaysOff agendaData mo dol ito m d it du
    | verifyIT it du && not (verifyAlreadyScheduled agendaData m d it du) && (dayOff yearDaysOff m d) = deleteSchedule yearDaysOff (agendaData ++ [(m,d,it,du)]) mo dol ito 
    | otherwise = agenda yearDaysOff agendaData

readReSchedule yearDaysOff agendaData = do

     -- Month
    putStrLn "Antigo Mes:"
    mo <- getLine
    -- Day 
    putStrLn "Antigo Dia:"
    dol <- getLine
    -- Schedule Initial Time
    putStrLn "Antigo Horário de início:"
    ito <- getLine


     -- Month
    putStrLn "Novo Mes:"
    m <- getLine
    -- Day 
    putStrLn "Novo Dia:"
    d <- getLine
    -- Schedule Initial Time
    putStrLn "Novo Horário de início:"
    it <- getLine
    -- Schedule Duration
    putStrLn "Nova Duração:"
    dur <- getLine

    reSchedule yearDaysOff agendaData (read mo :: Int) (read dol :: Int) (read ito :: Int) (read m :: Int) (read d :: Int) (read it :: Int) (read dur :: Int)

-- ############################ DELETE SCHEDULE ############################

deleteSchedule yearDaysOff agendaData m d it = agenda yearDaysOff [x | x <- agendaData,not (((getM x) == m) && ((getD x) == d) && ((getIT x) == it))]

readDeleteSchedule yearDaysOff agendaData = do
    -- Month
    putStrLn "Mes:"
    m <- getLine
    -- Day 
    putStrLn "Dia:"
    d <- getLine
    -- Schedule Initial Time
    putStrLn "Horário de início:"
    it <- getLine

    deleteSchedule yearDaysOff agendaData (read m :: Int) (read d :: Int) (read it :: Int)


-- ############################ MINIMUN SCHEDULE ############################

getMinValid yearDaysOff agendaData 12 31 18 dur = return (30,40,50,60)
getMinValid yearDaysOff agendaData m 31 18 dur = getMinValid yearDaysOff agendaData (m+1) 1 8 dur
getMinValid yearDaysOff agendaData m d 18 dur = getMinValid yearDaysOff agendaData m (d+1) 8 dur
getMinValid yearDaysOff agendaData m d it dur
    | (verifyIT it dur && not (verifyAlreadyScheduled agendaData m d it dur) && (dayOff yearDaysOff m d)) = return (m,d,it,dur)
    | otherwise = getMinValid yearDaysOff agendaData m d (it+1) dur

readInsertMinSchedule yearDaysOff agendaData = do
    -- Month
    putStrLn "Mes:"
    m <- getLine
    -- Day 
    putStrLn "Dia:"
    d <- getLine
    -- Schedule Duration
    putStrLn "Duração:"
    dur <- getLine

    schedule <- getMinValid yearDaysOff agendaData (read m :: Int) (read d :: Int) 8 (read dur :: Int)

    insertScheduleAux yearDaysOff agendaData (getM schedule) (getD schedule) (getIT schedule) (getDU schedule)

-- ############################ MINIMUN INTERVAL SCHEDULE ############################

-- ############################ MAX INTERVAL SCHEDULE ############################

-- ############################ READING THE YEAR DAYS OFF ############################

-- map read $ words string :: [Int] faz "1 2 3" virar [1,2,3]
stringToList string = map read $ words string :: [Int]

-- Test whether the day is in the list of days off
testPosition d (x:ds)
    | d == x = False
    | (null ds) = True
    | otherwise = testPosition d ds 

convertToDayOff string = [testPosition d (stringToList string) | d <- [1..31]]

-- True = Util Day , False = Holiday or weekend
readDayOffs ds = [convertToDayOff x| x <- ds]

divideYearInput (x:xs) = (x,readDayOffs xs)

-- Read Year
readYear = do 
    content <- readFile "calendario.txt"
    --putStrLn (show (lines (content)))
    return (divideYearInput (lines (content)))

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
        
        -- linhas <- lines (content)

        -- putStrLn (show (linhas))
        
        -- putStrLn (show (takeWhile (/="") (lines (content))))

        -- putStrLn (show (readMonths (lines (content))))

        -- putStrLn (show (readAgenda (readMonths (lines (content)))))
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
