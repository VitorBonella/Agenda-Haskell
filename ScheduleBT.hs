module ScheduleBT where

import Schedule

data ScheduleTree = Leaf | Node Schedule ScheduleTree ScheduleTree

emptyScheduleTree :: ScheduleTree
emptyScheduleTree = Leaf

-- ############################ Insert ############################

insert :: ([Char],[[Bool]]) -> Schedule -> ScheduleTree -> ScheduleTree
insert yearDaysOff schedule Leaf = if isValid yearDaysOff schedule then Node schedule Leaf Leaf else Leaf
insert yearDaysOff schedule (Node s left right)
    | schedule < s = if isValid yearDaysOff schedule && not (checkConflict schedule s) then Node s (insert yearDaysOff schedule left) right else Node s left right
    | schedule == s = Node s left right -- do not insert schedule if it is equal to an existing schedule
    | schedule > s = if isValid yearDaysOff schedule && not (checkConflict schedule s) then Node s left (insert yearDaysOff schedule right) else Node s left right

isValid :: ([Char],[[Bool]]) -> Schedule -> Bool
isValid yearDaysOff schedule = validTime (initialTime schedule) (duration schedule) && (dayOff yearDaysOff (month schedule) (day schedule)) && (verifyMonthDay (month schedule) (day schedule) yearDaysOff)

validTime :: Int -> Int -> Bool
validTime h d = (h >= 8 && h < 12) || (h >= 14 && h < 18) || ((calculateEndTime h d) <= 18)

checkConflict :: Schedule -> Schedule -> Bool
checkConflict s1 s2 = (conflictTime s1 s2 || conflictTime s2 s1) && (day s1 == day s2) && (month s1 == month s2)  
    where
        conflictTime schedule1 schedule2 =
            let endTime = (calculateEndTime (initialTime schedule1) (duration schedule1))
            in initialTime schedule2 < endTime && initialTime schedule2 > initialTime schedule2

calculateEndTime :: Int -> Int -> Int
calculateEndTime initialTime duration =
    if initialTime < 12 then
        if initialTime + duration > 12 then
            14 + (initialTime + duration - 12)
        else
            initialTime + duration
    else
        initialTime + duration


dayOff yearDaysOff m d 
    | m <=12 && d <= 31 = ((snd yearDaysOff) !! (m-1)) !! (d-1)
    | otherwise = False

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
verify yearDaysOff schedule (Node s left right)
    | schedule < s = if isValid yearDaysOff schedule && not (checkConflict schedule s) then verify yearDaysOff schedule left else False
    | schedule == s = False
    | schedule > s = if isValid yearDaysOff schedule && not (checkConflict schedule s) then verify yearDaysOff schedule right else False

-- ############################ Delete ############################

delete :: Int -> Int -> Int -> ScheduleTree -> ScheduleTree
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
    show (Node schedule left right) = (show schedule) ++ "\n" ++ (show left) ++ (show right)

-- ############################ Search ############################

searchSchedule :: ScheduleTree -> Int -> Int -> Int -> Maybe Schedule
searchSchedule Leaf _ _ _ = Nothing
searchSchedule (Node (Schedule d m h dur t dsc) left right) month day time
    | (month == m) && (day == d) && (time == h) = Just (Schedule d m h dur t dsc)
    | (month > m) || (month == m && day > d) || (month == m && day == d && time > h) = searchSchedule right month day time
    | otherwise = searchSchedule left month day time