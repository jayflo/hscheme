module Lists (

) where

last :: [a] -> Maybe a
last [] = Nothing
last [x] = Just x
last (x:xs) = Lists.last xs

penultimate :: [a] -> Maybe a
penultimate [] = Nothing
penultimate [x] = Nothing
penultimate [x, y] = Just x
penultimate (x:xs) = penultimate xs

nth :: Int -> [a] -> Maybe a
nth _ [] = Nothing
nth 0 xs = Just $ head xs
nth n ys@(x:xs) = if n < 0 || n >= Prelude.length ys then Nothing else nth (n-1) xs

length :: [a] -> Int
length = foldr (\x y -> 1 + y) 0

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = Lists.reverse xs ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == Lists.reverse xs

flatten :: [[a]] -> [a]
flatten = foldr (++) []

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:ys)
  | x == y = compress yys
  | x /= y = x : compress yys
  where yys = y : ys

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack xs = let t = span (head xs ==) xs
          in fst t : pack (snd t)

encode :: (Eq a) => [a] -> [(a,Int)]
encode = fmap (\x -> (head x, Prelude.length x)) . pack

decode :: [(a,Int)] -> [a]
decode = flip (>>=) (\x -> replicate (snd x) (fst x))

encodeDirect :: (Eq a) => [a] -> [(a,Int)]
encodeDirect [] = []
encodeDirect xs =
  let t = span (head xs ==) xs
  in (head (fst t), Prelude.length (fst t)) : encodeDirect (snd t)

duplicate :: Int -> [a] -> [a]
duplicate _ [] = []
duplicate 0 _ = []
duplicate n xs = xs >>= replicate n

dropn :: Int -> [a] -> [a]
dropn _ [] = []
dropn n xs = if n < 1 then xs
             else fst <$> filter (\t -> mod (snd t) n == 0) (zip xs [1,2..])

split :: Int -> [a] -> ([a],[a])
split n xs
  | n < 1 = ([], xs)
  | n <= Prelude.length xs = (take n xs, drop n xs)
  | n > Prelude.length xs = (xs, [])

slice :: Int -> Int -> [a] -> [a]
slice n m = fst . split (m-n) . snd. split n

rotate :: Int -> [a] -> [a]
rotate n xs = let t = split n xs
              in snd t ++ fst t

removeAt :: Int -> [a] -> [a]
removeAt n xs = let t = split n xs
                    st = snd t
                in fst t ++ if not (null st) then tail st else []

insertAt :: Int -> a -> [a] -> [a]
insertAt n x xs = let t = split n xs
                  in fst t ++ (x : snd t)

range :: Int -> Int -> [Int]
range n m = [n..m]


