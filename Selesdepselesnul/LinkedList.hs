-- author : Moch Deden(https://github.com/selesdepselesnul)
module Selesdepselesnul.LinkedList (
    LinkedList(..)
    , display
    , deleteHead
    , isIn
) where

infixr 5 :-:
data LinkedList a = EmptyList | a :-: LinkedList a
    deriving(Show)

display :: Show a => LinkedList a -> IO ()
display EmptyList = putStrLn "Kosong"
display (x :-: EmptyList) =  print x
display (x :-: xs) = print x >> display xs

deleteHead :: LinkedList a -> LinkedList a
deleteHead (x :-: xs) = xs
deleteHead _ = EmptyList

isIn :: Eq a => a -> LinkedList a -> Bool
_ `isIn` EmptyList = False
a `isIn` (x :-: EmptyList) = a == x
a `isIn` (x :-: xs) = x == a ||  a `isIn` xs
