module Menu where

import ScheduleBT
import ScheduleIO
import AgendaFiles

menu :: ([Char],[[Bool]]) -> ScheduleTree -> IO ()
menu yearDaysOff tree = do
    putStrLn "0 - Sair"
    putStrLn "1 - Recuperar agenda"
    putStrLn "2 - Verificar disponibilidade de horário"
    putStrLn "3 - Inserir compromisso no horário"
    putStrLn "4 - Inserir compromisso mais breve"
    putStrLn "5 - Inserir compromisso no intervalo mínimo"
    putStrLn "6 - Inserir compromisso no intervalo máximo"
    putStrLn "7 - Cancelar compromisso"
    putStrLn "8 - Reagendar compromisso"
    putStrLn "9 - Gravar agenda"
    putStrLn "Opcao: "
    option <- getLine
    case option of
        "0" -> return ()

        "1" -> do
            tree <- readCalendar yearDaysOff
            menu yearDaysOff tree

        "2" -> do
            checkAvailability yearDaysOff tree
            menu yearDaysOff tree

        "3" -> do
            schedule <- readSchedule
            let newTree = insert yearDaysOff schedule tree
            menu yearDaysOff newTree

        "4" -> do
            newTree <- readInsertMinSchedule yearDaysOff tree
            menu yearDaysOff newTree

        "5" -> do
            newTree <- readInsertMinIntervalSchedule yearDaysOff tree
            menu yearDaysOff newTree

        "6" -> do
            newTree <- readInsertMaxIntervalSchedule yearDaysOff tree
            menu yearDaysOff newTree

        "7" -> do
            newTree <- deleteSchedule tree
            menu yearDaysOff newTree

        "8" -> do
            newTree <- reschedule yearDaysOff tree
            menu yearDaysOff newTree

        "9" -> do
            putStrLn "Schedule Tree:"
            -- putStrLn (show tree)
            writeCalendar tree
            menu yearDaysOff tree
        
        _ -> do
            putStrLn "Invalid option. Please try again."
            menu yearDaysOff tree