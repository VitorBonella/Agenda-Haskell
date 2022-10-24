import Menu

-- https://gist.github.com/bfb/3855780

type Hour = Int
type Time = Int

type Scheduling = (Hour,Time)

type Day = (Int, [Scheduling)])

type Month = Int
type Agenda = (Bool, Month, [Day])

type DayOff = (Month,Day)

type Year = (Bool, [DayOff])


AvailableTime :: Agenda -> Year -> Day -> Month -> Scheduling -> Bool


main = do
    showMenu
    op <- leOp
    main