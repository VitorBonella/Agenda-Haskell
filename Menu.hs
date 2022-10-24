module Menu where

leOp = do
    putStrLn "Opcao:"
    opcao <- getLine
    return (read opcao :: Int)

showMenu = do  
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
