double x = x + x 

quadruple x = double (double x)




average ns = sum ns `div` length ns

n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]
last' xs = head (reverse xs)



test = init [1..11]

test2 = take (length [1..11] - 1) [1..11]



-- print :: Show a => a -> IO ()

hoeveel a =
    if a > 10 then print "veel" else
        if a < 10 then print "heel veel" 
        else print "precies 10"


tester a 
    | a > 10 = print "veel"
    | a < 10 = print "heel veel"
        


-- sort [a] = length a
-- print length [1..10]

lekkerTellen = 
    let b = [1..10]
    in print (map (+1) b)
    

safetail :: [a] -> [a]

safetail x = if null x then []
    else tail x

-- safetail x 
--     | null x  = []
--     | not (null x) = tail x


jaja = 
    print (True || False)


en :: Bool -> Bool -> Bool
-- en a b=
--     if a == True && b == True then True
--         else False
en a b
    | a == True && b == True = True
    | otherwise = False 


-- wat [(x,y) ]

goedemorgen = tail "hallo"

pyths :: Int -> [(Int, Int, Int)]
pyths a = [(x, y, z) | x <- [1..a], y <- [1..a], z <- [1..a], x^2 + y^2 == z^2]
perfect :: Int -> [Int]
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]
perfect n =
    let j = init (factors n)
    in [xs | xs <- j, sum j == n ]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (init (factors x)) == x]

nohoor :: Ord a => a -> [a] -> Int
nohoor a xs = length [x | x <- xs, a > x ]

wats getal = go getal
go n
    | n == 0    = print 0
    | otherwise = do
        print n
        go (n - 1)