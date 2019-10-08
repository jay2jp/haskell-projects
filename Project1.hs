module Project1 where

div7or9 :: Integer -> Bool
div7or9 x  = 
	if(x `mod` 7 == 0 || x `mod` 9 == 0 )
		then True		
		else False

echo :: [Char] -> [Char]
echo [] = []
echo (x:xs) = [x,x] ++ echo xs

echons :: [Char] -> [Char]
echons [] = []
echons(x:xs) =
	if(x == ' ')
		then [' '] ++ echons xs
		else [x,x] ++ echons xs

countEvens :: [Integer] -> Integer
countEvens [] = 0
countEvens (x:xs) = 
	if (x `mod` 2 == 0)
		then 1 + countEvens xs
		else 0 + countEvens xs

centroid :: [(Double,Double)] -> (Double,Double)

centroid x = let n1 = (fst (helper1 x))
		 n2 = (snd (helper1 x))
		 di = (fromIntegral(length x))
	     in (n1/di,n2/di)


hailstone :: Integer -> Integer
hailstone 1  = 1
hailstone 0  = 0
hailstone x = 
	if (x == 1) then 1
	else if ((x `mod` 2) == 0) then 1 + hailstone(x `div` 2)
	else 1 + hailstone(3 * x + 1)

helper1 :: [(Double,Double)] -> (Double,Double)  
helper1 (x:xs) = if(xs /= [])
		then (fst x + (fst (helper1 xs)), snd x + (snd (helper1 xs)))
		else (fst x, snd x)   
