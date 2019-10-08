module Project3 where

import Control.Monad
import Control.Exception
import Data.List
import System.Timeout
import Data.Maybe

data RE a
    = Symbol a
    | Empty
    | RE a :+: RE a
    | RE a :|: RE a
    | Repeat (RE a)
    | Plus (RE a)
    deriving (Show, Read, Eq, Ord)

data ABC = A | B | C deriving (Show, Read, Eq, Ord)

-- The language for atMost2As includes exactly those strings in which A occurs
-- no more than twice
atMost2As :: RE ABC
atMost2As = (Repeat(Symbol B :|: Symbol C):+:(Symbol A :|: Empty):+:(Repeat(Symbol B :|: Symbol C)):+:(Symbol A :|: Empty):+:(Repeat(Symbol B :|: Symbol C)))

-- anyOf alts returns a regular expression that matches any of the symbols
-- given in alts (assuming alts is non-empty)
anyOf :: [a] -> RE a
anyOf x = anyOfhelp x

anyOfhelp :: [a] -> RE a
anyOfhelp (x:xs) = if length(x:xs) == 1 then Symbol x else Symbol x :|: anyOfhelp xs

-- repeats m n r returns a regular expression which matches r at least m but no
-- more than n times (assuming m <= n)
repeats :: Int -> Int -> RE a -> RE a
repeats 0 0 r = Empty
repeats 0 y r = Empty :|: (help 1 (y-1) r)
repeats x y r = help2 (y-x) (makeminimum x r) r 
	

help :: Int -> Int -> RE a -> RE a
help 1 y r = if (y == 0) then r else r :|: (help 1 (y-1) r):+: r

help2 :: Int -> RE a -> RE a -> RE a
help2 y b r = if(y == 0) then b else b :|: (help2 (y -1) (b:+:r) r)

makeminimum :: Int -> RE a -> RE a
makeminimum x r = if (x == 1) then r else r :+: (makeminimum (x-1) r)
				
-- matchEmpty r indicates whether r will accept the empty string
matchEmpty :: RE a -> Bool
matchEmpty x =  if minLength x == 0 then True else False 

-- minLength r returns the minimum length of any string that matches r
minLength :: RE a -> Integer
minLength (Repeat((Symbol a) :|: (Empty))) = 0
minLength (Plus((Symbol a) :|: (Empty))) = 0
minLength (Symbol a) = 1 
minLength (Repeat a) = 0
minLength (Plus a) = 1
minLength (Empty) = 0
minLength (a :+: b) = minLength a + minLength b
minLength (a :|: b) = min (minLength a) (minLength b)




-- maxLength r returns Just n if the maximum length of any string matched by
-- r is n, or Nothing if there is no upper limit
maxLength :: RE a -> Maybe Integer
maxLength (Symbol x) = Just 1
maxLength (Repeat(Empty)) = Just 0
maxLength (Repeat a) = Nothing
maxLength (Plus(Empty)) = Just 0
maxLength (Plus a) = Nothing
maxLength Empty = Just 0
maxLength (a :+: b) = Just (sum(catMaybes [maxLength a , maxLength b]))


maxLength (a :|: b) = max (maxLength a) (maxLength b)

maxGen :: RE a -> Integer
maxGen Empty = 0 
maxGen(Symbol x) = 1
maxGen (a :+: b) = (maxGen a + maxGen b)



-- firsts r returns a list containing every symbol that occurs first in some
-- string that matches r
firsts :: RE a -> [a]
firsts (Symbol x)  = [x]
firsts (Empty) = []
firsts (Repeat a) = firsts a
firsts (Plus x) = firsts x
firsts (((a):|:(Empty)):+:((b) :|: (Empty))) = (firsts a) ++ (firsts b)
firsts (((Empty):|:(a)):+:((b) :|: (Empty))) = (firsts a) ++ (firsts b)
firsts (((a):|:(Empty)):+:((Empty) :|: (b))) = (firsts a) ++ (firsts b)
firsts (((Empty):|:(a)):+:((Empty) :|: (b))) = (firsts a) ++ (firsts b)
firsts ( a  :+: ( (b) :|: (Empty)) :+: c) = firsts a
firsts(a :+: (b :|: (Empty))) = firsts a
firsts(a :+: b) = (firsts a) ++ (firsts b)


firsts(a :|: b) = (firsts a) ++ (firsts b)
firsts(a :|: Empty) = []
firsts(Empty :|: b) = []



matchPre :: (Eq a) => RE a -> [a] -> [(Bool,[a])]
matchPre (Symbol a) (x:xs) | x == a = [(True,xs)]
matchPre (Symbol a) _ = []
matchPre Empty xs = [(False,xs)]
matchPre (r :|: s) xs = matchPre r xs ++ matchPre s xs
matchPre (r :+: s) xs = do
		(r_consumed,ys) <- matchPre r xs 
		(s_consumed,zs) <- matchPre s ys
		return (r_consumed || s_consumed,zs)
matchPre (Repeat r) xs = (False,xs) : do
		(r_consumed,ys) <- matchPre r xs
		guard r_consumed
		(_,zs) <-matchPre (Repeat r) ys
		return (True,zs)
matchPre (Plus r) xs = do 
		(r_consumed, ys) <- matchPre r xs
		(s_consumed,zs) <- matchPre (Repeat r) ys
		return (r_consumed || s_consumed, zs)

match :: (Eq a) => RE a -> [a] -> Bool
match r = any (null.snd).matchPre r
