module ScheduleBT where

import Schedule

data ScheduleTree = Leaf | Node Schedule ScheduleTree ScheduleTree

emptyScheduleTree :: ScheduleTree
emptyScheduleTree = Leaf

-- ############################ Insert ############################

insert :: Schedule -> ScheduleTree -> ScheduleTree
insert schedule Leaf = if isValid schedule then Node schedule Leaf Leaf else Leaf
insert schedule (Node s left right)
    | schedule < s = if isValid schedule && not (checkConflict schedule s) then Node s (insert schedule left) right else Node s left right
    | schedule == s = Node s left right -- do not insert schedule if it is equal to an existing schedule
    | schedule > s = if isValid schedule && not (checkConflict schedule s) then Node s left (insert schedule right) else Node s left right

isValid :: Schedule -> Bool
isValid schedule = validTime (initialTime schedule) (duration schedule)

validTime :: Int -> Int -> Bool
validTime h d = (h >= 8 && h < 12) || (h >= 14 && h < 18) || ((calculateEndTime h d) <= 18)

checkConflict :: Schedule -> Schedule -> Bool
checkConflict s1 s2 = (conflictTime s1 s2 || conflictTime s2 s1) && (day s1 == day s2) && (month s1 == month s2)
    where
        conflictTime schedule1 schedule2 =
            let endTime = (calculateEndTime (initialTime schedule1) (duration schedule1))
            in initialTime schedule2 < endTime

calculateEndTime :: Int -> Int -> Int
calculateEndTime initialTime duration =
    if initialTime < 12 then
        if initialTime + duration > 12 then
            14 + (initialTime + duration - 12)
        else
            initialTime + duration
    else
        initialTime + duration

-- ############################ Verify ############################

verify :: Schedule -> ScheduleTree -> Bool
verify schedule Leaf = if isValid schedule then True else False
verify schedule (Node s left right)
    | schedule < s = if isValid schedule && not (checkConflict schedule s) then verify schedule left else False
    | schedule == s = False -- do not insert schedule if it is equal to an existing schedule
    | schedule > s = if isValid schedule && not (checkConflict schedule s) then verify schedule right else False

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