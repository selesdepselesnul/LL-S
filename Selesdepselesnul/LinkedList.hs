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

deleteAfter :: Eq a => a -> LinkedList a -> LinkedList a
deleteAfter _ EmptyList = EmptyList
deleteListAfter x (y :-: EmptyList) = y :-: EmptyList 
deleteListAfter x (y :-: z :-: zs) = if x == y then y :-: zs  else y :-:  (deleteListAfter x (z :-: zs))

insertAfter :: Eq a => a -> a -> LinkedList a -> LinkedList a
insertAfter _ _ EmptyList = EmptyList 
insertAfter searched x rest@(y :-: EmptyList)
    | searched /= y = rest 
insertAfter searched x (y :-: ys) 
    | searched == y = y :-: x :-: ys
    | otherwise = y :-: (insertAfter searched x ys)