module Project2 where
import Data.List.Split


zero :: Int -> Int -> [[Double]]
zero row column = replicate row . replicate column $ 0

ident :: Int -> [[Double]]
ident n = splitEvery n (idenhelp n n)

idenhelp :: Int -> Int -> [Double] -- one is the x one is the length
idenhelp 0 length = []
idenhelp x length = take (length-x)(repeat 0.0) ++ [1.0] ++ take (x-1) (repeat 0.0) ++ (idenhelp (x-1) length)



diag :: [[Double]] -> [Double]
diag x = diaghelp 0 x  -- All that is left is to reverse diaghelp im a littel to tire for it rn

diaghelp :: Int->[[Double]]->[Double]

diaghelp y (x:xs) = if length (x:xs) == 1 then [x!!0] else [x!!y] ++ if diaglen (x:xs) < y then [] else diaghelp (y+1) xs
--diaghelp len (x:xs) = [x!!(len)] ++ diaghelp (len-1) xs

diaglen :: [[Double]] -> Int
diaglen (x:xs) = (length x)-2


add :: [[Double]] -> [[Double]] -> [[Double]]
add [] [] = []
add (x : xs) (y : ys) = add2 x y : add xs ys

add2 :: [Double] -> [Double] -> [Double]
add2 [] [] = []
add2 (x : xs) (y : ys) = x + y : add2 xs ys

transp :: [[Double]] -> [[Double]]
transp ([] : _ ) = []
transp x = (map head x) : transp (map tail x)




data Sparse = Sparse Int Int [(Int,[(Int,Double)])]
    deriving (Show, Eq)

sident :: Int -> Sparse
sident x = Sparse x x (zip [0 .. x-1] (reverse . breaks . trans $ x))

trans :: Int  -> [(Int,Double)]
trans 0 = []
trans x = (x - 1, 1.0) : trans (x - 1)

breaks :: [a] -> [[a]]
breaks [] = []
breaks (x : xs) = (x : []) : breaks xs

sdiag :: Sparse -> [Double]
sdiag x = reverse(sdiag2 x)


sdiag2 :: Sparse -> [Double]
sdiag2 (Sparse row col []) = if col > row then take row(repeat 0.0) else take col(repeat 0.0)
sdiag2 (Sparse _ 1 list) = [snd(head(snd(head(list))))]
sdiag2 (Sparse 1 _ list) = [snd(last(snd(head(list))))]
sdiag2 (Sparse 0 _ _) = []
sdiag2 (Sparse _ 0 _ ) = []
sdiag2 (Sparse row col list) = if col-1 /= (checker list) then [0.0] ++ sdiag2 (Sparse (row-1) (col-1) list) else sdiaghelp col (last list) ++ sdiag2 (Sparse (row-1) (col-1) (init list) )

sdiaghelp :: Int->(Int,[(Int,Double)])->[Double]
sdiaghelp 0 x = []
sdiaghelp col x = if col-1 == fst x then  extract (snd x) else [0.0]

extract :: [(Int,Double)]-> [Double]
extract x = [snd(head x)]

checker :: [(Int,[(Int,Double)])]->Int
checker x = fst(last(x))





      



sadd :: Sparse -> Sparse -> Sparse
sadd = undefined