module ScheduleBT where

import Schedule
import Data.List(minimumBy,maximumBy)
import Data.Ord(comparing)

data ScheduleTree = Leaf | Node Schedule ScheduleTree ScheduleTree

emptyScheduleTree :: ScheduleTree
emptyScheduleTree = Leaf

-- ############################ Insert ############################

insert:: ([Char],[[Bool]]) -> Schedule -> ScheduleTree -> ScheduleTree
insert yearDaysOff schedule Leaf = if isValid yearDaysOff schedule then Node schedule Leaf Leaf else Leaf
insert yearDaysOff schedule (Node s left right)
    | schedule < s = if isValid yearDaysOff schedule && not (checkConflict schedule s) then Node s (insert yearDaysOff schedule left) right else Node s left right
    | schedule == s = Node s left right -- do not insert schedule if it is equal to an existing schedule
    | schedule > s = if isValid yearDaysOff schedule && not (checkConflict schedule s) then Node s left (insert yearDaysOff schedule right) else Node s left right

isValid:: ([Char],[[Bool]]) -> Schedule -> Bool
isValid yearDaysOff schedule = validTime (initialTime schedule) (duration schedule) && (dayOff yearDaysOff (month schedule) (day schedule)) && (verifyMonthDay (month schedule) (day schedule) yearDaysOff)

validTime:: Int -> Int -> Bool
validTime h d = ((h >= 8 && h < 12) || (h >= 14 && h < 18)) && ((calculateEndTime h d) <= 18)

checkConflict:: Schedule -> Schedule -> Bool
checkConflict s1 s2 = ((conflictTime s1 s2 || conflictTime s2 s1) && (day s1 == day s2) && (month s1 == month s2))  
    where
        conflictTime schedule1 schedule2 =
            let endTime = (calculateEndTime (initialTime schedule1) (duration schedule1))
            in initialTime schedule2 >= initialTime schedule1 && initialTime schedule2 < endTime

calculateEndTime:: Int -> Int -> Int
calculateEndTime initialTime duration =
    if initialTime < 12 then
        if initialTime + duration > 12 then
            14 + (initialTime + duration - 12)
        else
            initialTime + duration
    else
        initialTime + duration

dayOff:: ([Char],[[Bool]]) -> Int -> Int -> Bool
dayOff yearDaysOff m d 
    | m <=12 && d <= 31 = ((snd yearDaysOff) !! (m-1)) !! (d-1)
    | otherwise = False

verifyMonthDay:: Int -> Int -> ([Char],[[Bool]]) -> Bool
verifyMonthDay m d yearDaysOff
    | m == 1 && d <= 31 = True
    | m == 2 && d <= 29 && ((fst yearDaysOff) == "True") = True
    | m == 2 && d <= 28 = True
    | m == 3 && d <= 31 = True
    | m == 4 && d <= 30 = True
    | m == 5 && d <= 31 = True
    | m == 6 && d <= 30 = True
    | m == 7 && d <= 31 = True
    | m == 8 && d <= 31 = True
    | m == 9 && d <= 30 = True
    | m == 10 && d <= 31 = True
    | m == 11 && d <= 30 = True
    | m == 12 && d <= 31 = True
    | otherwise = False

-- ############################ Verify ############################

verify :: ([Char],[[Bool]]) -> Schedule -> ScheduleTree -> Bool
verify yearDaysOff schedule Leaf = if isValid yearDaysOff schedule then True else False
verify yearDaysOff schedule (Node s left right) =
  if (checkConflict schedule s)
    then False
    else verify yearDaysOff schedule left && verify yearDaysOff schedule right

-- ############################ Delete ############################

delete:: Int -> Int -> Int -> ScheduleTree -> ScheduleTree
delete day month time Leaf = Leaf
delete day month time (Node (Schedule d m h dur t dsc) left right)
    | day == d && month == m && time == h = merge left right
    | schedule < (Schedule day month time 0 Remote "" ) = Node (Schedule d m h dur t dsc) left (delete day month time right)
    | otherwise = Node (Schedule d m h dur t dsc) (delete day month time left) right
  where schedule = (Schedule d m h dur t dsc)
        merge Leaf Leaf = Leaf
        merge Leaf right = right
        merge left Leaf = left
        merge left@(Node leftS _ _) right@(Node rightS _ _)
            | leftS < rightS = Node leftS left (merge Leaf right)
            | otherwise = Node rightS (merge left Leaf) right

-- ############################ Show ############################

instance Show ScheduleTree where
    show Leaf = ""
    show (Node schedule left right) = (show left) ++ (show schedule) ++  "\n" ++ (show right)

-- ############################ Search ############################

searchSchedule :: ScheduleTree -> Int -> Int -> Int -> Maybe Schedule
searchSchedule Leaf _ _ _ = Nothing
searchSchedule (Node (Schedule d m h dur t dsc) left right) month day time
    | (month == m) && (day == d) && (time == h) = Just (Schedule d m h dur t dsc)
    | (month > m) || (month == m && day > d) || (month == m && day == d && time > h) = searchSchedule right month day time
    | otherwise = searchSchedule left month day time

-- ############################ In order conversion ############################

scheduleBtToScheduleList:: ScheduleTree -> [Schedule] 
scheduleBtToScheduleList Leaf = []
scheduleBtToScheduleList (Node schedule left right) = scheduleBtToScheduleList left ++ [schedule] ++ scheduleBtToScheduleList right

-- ############################ Insert Soon as Possible ############################

returnMinSchedule:: ([Char],[[Bool]]) -> ScheduleTree -> Schedule -> Maybe Schedule
returnMinSchedule yearDaysOff tree schedule@(Schedule 31 1 18 _ _ _) = returnMinSchedule yearDaysOff tree (addMonthReset schedule)
returnMinSchedule yearDaysOff tree schedule@(Schedule 28 2 18 _ _ _) = returnMinSchedule yearDaysOff tree (addMonthReset schedule)
returnMinSchedule yearDaysOff tree schedule@(Schedule 31 3 18 _ _ _) = returnMinSchedule yearDaysOff tree (addMonthReset schedule)
returnMinSchedule yearDaysOff tree schedule@(Schedule 30 4 18 _ _ _) = returnMinSchedule yearDaysOff tree (addMonthReset schedule)
returnMinSchedule yearDaysOff tree schedule@(Schedule 31 5 18 _ _ _) = returnMinSchedule yearDaysOff tree (addMonthReset schedule)
returnMinSchedule yearDaysOff tree schedule@(Schedule 30 6 18 _ _ _) = returnMinSchedule yearDaysOff tree (addMonthReset schedule)
returnMinSchedule yearDaysOff tree schedule@(Schedule 31 7 18 _ _ _) = returnMinSchedule yearDaysOff tree (addMonthReset schedule)
returnMinSchedule yearDaysOff tree schedule@(Schedule 31 8 18 _ _ _) = returnMinSchedule yearDaysOff tree (addMonthReset schedule)
returnMinSchedule yearDaysOff tree schedule@(Schedule 30 9 18 _ _ _) = returnMinSchedule yearDaysOff tree (addMonthReset schedule)
returnMinSchedule yearDaysOff tree schedule@(Schedule 31 10 18 _ _ _) = returnMinSchedule yearDaysOff tree (addMonthReset schedule)
returnMinSchedule yearDaysOff tree schedule@(Schedule 30 11 18 _ _ _) = returnMinSchedule yearDaysOff tree (addMonthReset schedule)
returnMinSchedule yearDaysOff tree schedule@(Schedule 31 12 18 _ _ _) = Nothing
returnMinSchedule yearDaysOff tree schedule@(Schedule _ _ 18 _ _ _) = returnMinSchedule yearDaysOff tree (addDayReset schedule) --novo dia
returnMinSchedule yearDaysOff tree schedule
    | verify yearDaysOff schedule tree = Just schedule
    | otherwise = returnMinSchedule yearDaysOff tree (addAnHour schedule)

-- ############################ Min Interval ############################

hourDistance:: Int -> Int -> Int -> Int -> Int
hourDistance it1 du1 it2 du2
    | it2 > 12 && it1 < 12 = it2 - (it1+du1) - 2
    | it1 > 12 && it2 < 12 = (18 - (it1+du1)) + (it2 - 8)
    | otherwise = it2 - (it1+du1)

daysDistance:: ([Char],[[Bool]]) -> Int -> Int -> Int -> Int
daysDistance yearDaysOff m d1 d2 = (length [x | x <- [d1+1..d2],(dayOff yearDaysOff m x)]) * 8

distanceBetweenSchedule:: ([Char],[[Bool]]) -> Schedule -> Schedule -> Int -> Int
distanceBetweenSchedule yearDaysOff schedule1 schedule2 dist
    | (month schedule1) == (month schedule2) && (day schedule1) == (day schedule2) = (dist + (hourDistance (initialTime schedule1) (duration schedule1) (initialTime schedule2) (duration schedule2)))
    | (month schedule1) == (month schedule2) = (dist + (hourDistance (initialTime schedule1) (duration schedule1)  (initialTime schedule2) (duration schedule2)) + (daysDistance yearDaysOff (month schedule1) (day schedule1) (day schedule2)))
    | otherwise = dist

pairs:: [a] -> [(a,a)]
pairs [] = []
pairs (x:[]) = []
pairs (x:y:zs) = (x, y) : pairs (y : zs)

allIntervals:: ([Char],[[Bool]]) -> [Schedule] -> [Int]
allIntervals yearDaysOff scheduleList = [distanceBetweenSchedule yearDaysOff s1 s2 0 | (s1,s2) <- (pairs scheduleList)]

verifyMonthDayAux:: ([Char],[[Bool]]) -> Int -> Int -> IO Bool
verifyMonthDayAux yearDaysOff m d  = return (verifyMonthDay m d yearDaysOff)

selectMonthAndDays:: Int -> Int -> Int -> [Schedule] -> [Schedule]
selectMonthAndDays m d max_days scheduleList = [x | x <- scheduleList, (month x) == m && (day x) <= d+max_days]

getTheMinimumIndex [] dur = []
getTheMinimumIndex xs dur = ([(i,d) | (i,d) <- zip [0..] xs, d >= dur])

returnMinIndex:: [(Int,Int)] -> Maybe (Int,Int)
returnMinIndex xs
    | (null xs) = Nothing
    | otherwise = Just (minimumBy (comparing snd) xs)

-- ############################ Max Interval ############################

returnMaxIndex:: [(Int,Int)] -> Maybe (Int,Int)
returnMaxIndex xs
    | (null xs) = Nothing
    | otherwise = Just (maximumBy (comparing snd) xs)
