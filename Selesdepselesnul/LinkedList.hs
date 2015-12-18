-- author : Moch Deden(https://github.com/selesdepselesnul)
module Selesdepselesnul.LinkedList (
    LinkedList(..)
    , display
    , deleteHead
    , isIn
    , deleteAfter
    , insertAfter
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

deleteAfter :: Eq a => a -> LinkedList a -> LinkedList a
deleteAfter _ EmptyList = EmptyList
deleteAfter pivot list@(y :-: EmptyList) = list
deleteAfter pivot (x :-: y :-: ys) = 
    if pivot == x then x :-: ys  else x :-: (deleteAfter pivot $ y :-: ys)

insertAfter :: Eq a => a -> a -> LinkedList a -> LinkedList a
insertAfter _ _ EmptyList = EmptyList 
insertAfter pivot x list@(y :-: EmptyList)
    | pivot /= y = list 
insertAfter pivot x (y :-: ys) 
    | pivot == y = y :-: x :-: ys
    | otherwise = y :-: (insertAfter pivot x ys)