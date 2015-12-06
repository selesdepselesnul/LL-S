-- author  : Moch Deden(https://github.com/selesdepselesnul)

module Main where
import Selesdepselesnul.LinkedList
import System.Process
import System.Info as Info

main :: IO ()
main = do
    askMenu EmptyList

askMenu :: LinkedList String -> IO ()
askMenu linkedList =
    displayMenu >> getLine >>= \x -> clear >> handleChoice x

    where
        displayMenu = do
            putStrLn "Masukan pilihan : "
            putStrLn "1. Insert"
            putStrLn "2. Search"
            putStrLn "3. Delete"
            putStrLn "4. Display"
            putStrLn "5. Exit"
            putStrLn "->"

        clear = do
            if Info.os == linux || Info.os == osX then
                system "clear"
            else
                system "cls"
            return ()
            where
                linux = "linux"
                osX = "darwin"

        handleChoice choice
            | choice == "1" = do
                putStrLn "Masukan nilainya -> "
                newValue <- getLine
                putStrLn $ "Anda memasukan nilai -> " ++ newValue
                askMenu $ newValue :-: linkedList
            | choice == "2" = do
                putStrLn "Masukan nilainya yang hendak dicari : "
                searchedValue <- getLine
                if searchedValue `isIn` linkedList then
                    putStrLn "Ditemukan"
                else
                    putStrLn "Tidak ditemukan"
                askMenu linkedList
            | choice == "3" = do
                putStrLn "Bagian head dihapus, isi linked-list sekarang : "
                let newLinkedList = deleteHead linkedList
                display newLinkedList
                askMenu newLinkedList
            | choice == "4" = do
                putStrLn "Isi linked-list :"
                display linkedList
                askMenu linkedList
            | choice == "5" = putStrLn "Program Keluar!"
            | otherwise = do
                putStrLn "Pilihan tidak valid!"
                askMenu linkedList

