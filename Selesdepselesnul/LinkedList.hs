-- author : Moch Deden(https://github.com/selesdepselesnul)
module Selesdepselesnul.LinkedList
    (
    LinkedList(..)
    , display
    , deleteHead
    , search
    ) where
infixr 5 :-:
data LinkedList a =
    EmptyList
    | a :-: LinkedList a
    deriving(Show)

display :: LinkedList String -> IO ()
display EmptyList = putStrLn "Kosong"
display (x :-: EmptyList) = putStrLn  x
display (x :-: xs) = putStrLn x >> display xs

deleteHead :: LinkedList String -> LinkedList String
deleteHead (x :-: xs) = xs
deleteHead _ = EmptyList

search :: String -> LinkedList String -> Bool
search _ EmptyList = False
search a (x :-: EmptyList) = a == x
search a (x :-: xs) = x == a || search a xs
