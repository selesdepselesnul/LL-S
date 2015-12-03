-- author  : Moch Deden(https://github.com/selesdepselesnul)
module Main where
import Selesdepselesnul.LinkedList
import System.Process
import System.Info as Info

clear = 
    if Info.os == "linux" || Info.os == "darwin" then
        system "clear"
    else
        system "cls"

askMenu :: LinkedList String -> IO ()
askMenu linkedList = do
    putStrLn "Masukan pilihan : "
    putStrLn "1. Insert"
    putStrLn "2. Search"
    putStrLn "3. Delete"
    putStrLn "4. Display"
    putStrLn "5. Exit"
    putStrLn "-> "
    choice <- getLine
    clear
    handleChoice choice
    where handleChoice x
            | x == "1" = do
                putStrLn $ "Masukan nilainya -> "
                newValue <- getLine
                putStrLn $ "Anda memasukan nilai -> " ++ newValue
                askMenu $ newValue :-: linkedList
            | x == "2" = do
                putStrLn "Masukan nilainya yang hendak dicari : "
                searchedValue <- getLine
                if search searchedValue linkedList then
                    putStrLn "Ditemukan"
                else
                    putStrLn "Tidak ditemukan"
                askMenu linkedList
            | x == "3" = do
                putStrLn "Bagian head dihapus, isi linked-list sekarang : "
                let newLinkedList = deleteHead linkedList
                display newLinkedList
                askMenu newLinkedList
            | x == "4" = do
                putStrLn "Isi linked-list :"
                display $ linkedList
                askMenu linkedList
            | x == "5" = putStrLn "Program Keluar!"
            | otherwise = putStrLn "Pilihan tidak valid!" >> askMenu linkedList

main :: IO ()
main = do
    askMenu EmptyList
