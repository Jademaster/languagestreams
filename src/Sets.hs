{-# LANGUAGE ExistentialQuantification #-}
module Sets where

import Data.Stream

-- functions are a source target and a partially defined function
data Function x y = Function ([x]) (x -> Maybe y) ([y]) 
domain :: Function x y -> [x]
domain (Function a b c) = a
range :: Function x y -> [y]
range (Function a b c) = c
func :: Function x y -> (x -> Maybe y)
func (Function a b c) = b

instance Show x => Show (Function x y) where
    show (Function a b c) = show a

-- these functions represent the source and target maps of a graph. I universall quantified over
-- e because mult changes the type of the edges of a graph
data Graph v = forall e. Graph (Function e v) (Function e v)
vertices :: Graph v -> [v]
vertices (Graph s t) = range s

-- this code doesn't work because the edge type is hidden
--edges :: Graph String -> [String]
--edges (Graph s t) = concat ( domain s )




mult :: Eq v => Graph v -> Graph v -> Graph v
mult (Graph s1 t1) (Graph s2 t2) = Graph (Function e s x) (Function e t x) where
    e = [ (i,j) | i <- domain s1, j <- domain s2, (func t1) i == (func s2) j ]
    x = range s1
    s (a,b) = func s1 a
    t (a,b) = func t2 b

-- the language of a graph is a stream of its time n executions.
language :: Eq v => Graph v -> Stream (Graph v)
language g =  Cons i (language (mult g i)) where
    i = Graph (Function (vertices g) (return . id) (vertices g)) (Function (vertices g) (return . id) (vertices g))

    
-- an example. a1 is a self loop on b1, a2 goes from b1 to b2, and a3 goes from b2 to b1
s = Function ["a1", "a2", "a3"] f ["b1", "b2"] where
    f "a1"= Just "b1"
    f "a2" = Just "b1"
    f "a3" = Just "b2"
    f  _   = Nothing

t = Function ["a1", "a2", "a3"] f ["b1", "b2"] where
    f "a1"= Just "b1"
    f "a2" = Just "b2"
    f "a3" = Just "b1"
    f  _   = Nothing

g = Graph s t
h= language g
