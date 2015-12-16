-- author  : Moch Deden(https://github.com/selesdepselesnul)

module Main where
import Selesdepselesnul.LinkedList
import System.Process
import System.Info as Info

main :: IO ()
main = do
    askMenu EmptyList

askMenu :: LinkedList String -> IO ()
askMenu linkedList = do
    displayMenu 
    choice <- getLine 
    clear
    handleChoice choice

    where
        displayMenu = do
            putStrLn "Masukan pilihan : "
            putStrLn "1. Insert first"
            putStrLn "2. Search"
            putStrLn "3. Delete first"
            putStrLn "4. Display"
            putStrLn "5. Insert after"
            putStrLn "6. Delete after"
            putStrLn "7. Exit"
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
            | choice == "5" = do
                putStrLn "Masukan nilai yang menjadi pivot : "
                pivot <- getLine
                putStrLn "Masukan nilai yang akan di insert : "
                value <- getLine
                askMenu $ insertAfter pivot value linkedList
            | choice == "6" = do
                putStrLn "Masukan nilai yang menjadi pivot : "
                pivot <- getLine
                askMenu $ deleteAfter pivot linkedList
            | choice == "7" = putStrLn "Program Keluar!"
            | otherwise = do
                putStrLn "Pilihan tidak valid!"
                askMenu linkedList

