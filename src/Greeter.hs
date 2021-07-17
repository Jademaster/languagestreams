module Greeter where

import Data.Text.Titlecase
greet :: String -> String
greet who = titlecase $ "Hello, " ++ who ++ "!"
-- should make these classes, maybe later

-- graphs
data State = A | B | C deriving (Eq, Show)
data Action = D | E deriving (Eq, Show)
data Edge = Edge State Action State deriving (Eq, Show)

source :: Edge -> State
source (Edge x a y) = x
target :: Edge -> State 
target (Edge x a y) = y
label :: Edge -> Action
label (Edge x a y) = a

data Graph = Graph [Edge] deriving (Eq, Show)

-- transition systems

data Trans = Trans (State -> Action -> State)

fire :: Trans -> State -> Action -> Edge
fire (Trans f) x a = Edge x a (f x a) 

run :: Trans -> State -> [Action] -> [Edge]
run t i (a:as) = edge : run t (target edge) as where
    edge = fire t i a 
run t i [] = []

-- going betwixt

addedge :: Edge -> Trans -> Trans
addedge (Edge x a y) (Trans f) = Trans g where
   g b c 
      | b == x && c == a = y
      | otherwise = f b c

translate :: Graph -> Trans
translate (Graph (edge : edges)) = addedge edge (translate (Graph edges))
translate (Graph [] ) = Trans g where
   g a b = a

-- streams of graphs 
data Stream a = Cons a (Stream a)
compile :: Graph -> Int -> Graph

-- an example
t = Trans g where
   g a b = a

g = Graph [ Edge A D B, Edge B D C, Edge C E A ]












