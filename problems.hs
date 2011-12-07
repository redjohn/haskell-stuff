import Data.List (group)

-- Problem 1
myLast = head . reverse

-- Problem 2
myButLast = head . tail . reverse

-- Problem 3
elementAt i xs = xs !! (i - 1)

-- Problem 4
myLength xs = sum [ 1 | x <- xs ]

-- Problem 5
myReverse = foldl (flip (:)) []

-- Problem 6
isPalindrome xs = xs == (reverse xs)

-- Problem 8
compress :: Eq a => [a] -> [a]
compress = map head . group

-- Problem 9
pack :: Eq a => [a] -> [[a]]
pack = foldr addItem []
    where addItem item []         = [[item]]
          addItem item acc@(x:xs) = if item `elem` x
                                    then (item : x) : xs
                                    else [item] : acc

-- Problem 10
encode :: Eq a => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . pack

-- Problem 14
dupItems xs = concat [ [x, x] | x <- xs ]

-- Problem 15
replicateItems n xs = concat [ take n $ repeat x | x <- xs ]

---- Could be made a little nice by using replicate instead
---- of take and repeat

-- Problem 16
dropEvery n xs = [ a | (a, b) <- zip xs markers, b ]
    where markers = concat $ repeat $ replicate (n - 1) True ++ [False]

---- An alternative; when writing the above solution,
---- I wasn't aware of the cycle function. However, the above
---- solution handles the case where n == 0 without needing
---- an extra clause (because replicate handles n <= 0). This
---- probably doesn't matter much since the other solutions
---- don't seem to worry about handling this case.
dropEvery' 0 _  = []
dropEvery' n xs = [ a | (a, b) <- zip xs $ cycle [1..n], b /= n ]

-- Problem 17
-- split n xs = (take n xs, drop n xs)
--Doh! Not supposed to use predefined predicates

split n xs = (extractItems (<= n), extractItems (> n))
    where extractItems p = [ x | (x, i) <- indexedItems, p i ]
          indexedItems = zip xs [1..]

-- Problem 18
slice start end xs = map (xs !!) [(start - 1)..(end - 1)]

---- The above may not be the most efficient (but I don't
---- really know). Something using zip like this might
---- be better.
slice' start end xs = [ x | (x, i) <- zip xs [1..end], i >= start ]

-- Problem 19
rotate 0 xs = xs
rotate n xs = first ++ second
    where splitter = if n < 0 then length xs + n else n
          (second, first) = split splitter xs

-- Problem 20
removeAt n xs = (dropped, concat [first, second])
    where (first, dropped:second) = split n xs
