module Lists (

) where

import           Control.Arrow
import           Prelude       hiding (last, length, reverse)
import qualified Prelude       as P
import qualified System.Random as R

-- P01
last :: [a] -> Maybe a
last [] = Nothing
last [a] = Just a
last (a:as) = last as

-- P02
penultimate :: [a] -> Maybe a
penultimate as
  | P.length as < 2 = Nothing
  | otherwise = Just $ (P.last . init) as

-- P03
nth :: Int -> [a] -> Maybe a
nth _ [] = Nothing
nth n as | n < 1 || n > P.length as = Nothing
nth 1 as = Just $ head as
nth n (a:as) = nth (n-1) as

-- P04
length :: [a] -> Int
length = foldr (curry (uncurry (+) . first (const 1))) 0

-- P05
reverse :: [a] -> [a]
reverse [] = []
reverse (a:as) = reverse as ++ [a]

-- P06
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome as = as == reverse as

-- P07
flatten :: [[a]] -> [a]
flatten = foldr (++) []

-- P08
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress as = foldr (\a b -> if a == head b then b else a:b) [] as

-- P09
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack as = uncurry (:) $ second pack $ span (head as ==) as

-- P10, P11
encode :: (Eq a) => [a] -> [(a,Int)]
encode = fmap (head &&& P.length) . pack

-- P12
decode :: [(a,Int)] -> [a]
decode = (=<<) (uncurry (flip replicate))

-- P13
encodeDirect :: (Eq a) => [a] -> [(a,Int)]
encodeDirect [] = []
encodeDirect as = uncurry (:) $
                  (head &&& P.length) *** encodeDirect $
                  span (head as ==) as

-- P14, P15
duplicate :: Int -> [a] -> [a]
duplicate _ [] = []
duplicate 0 _ = []
duplicate n as = as >>= replicate n

-- P16
dropn :: Int -> [a] -> [a]
dropn _ [] = []
dropn n as
  | n < 1 = as
  | otherwise = fst <$> filter ((0 ==) . mod n . snd) (zip as [1,2..])

  -- P17
split :: Int -> [a] -> ([a],[a])
split n as
  | n < 1 = ([], as)
  | n < len = (take n as, drop n as)
  | n >= len = (as, [])
  where len = P.length as

  -- P18
slice :: Int -> Int -> [a] -> [a]
slice n m = fst . split (m-n) . snd. split n

-- P19
rotate :: Int -> [a] -> [a]
rotate n = uncurry (++) . (snd &&& fst) . split n

-- P20
removeAt :: Int -> [a] -> [a]
removeAt n as
  | null (snd t) = fst t
  | otherwise = uncurry (++) $ (fst &&& tail . snd) t
  where t = split (n-1) as

  -- P21
insertAt :: Int -> a -> [a] -> [a]
insertAt n a = uncurry (++) . second ((:) a) . split n

-- P22
range :: Int -> Int -> [Int]
range n m = [n..m]


