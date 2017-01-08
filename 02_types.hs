foo :: Int -> Int
foo 0 = 3
foo x = x+1

bar :: Integer -> Integer
bar x = x+1

zoi :: Float -> Float
zoi x = x+1

tar :: Double -> Double
tar x = x+1

yup :: (a,b,a) -> [a]
yup (x,_,z) = [x,z]

eqc :: (Eq a) => a->a->Bool
eqc a b = a == b && b == a

ori :: (Ord a) => a->a->a->a->[[a]]
ori w x y z = [ [x,y] | x <- [w, x,y,z], y <- [w, x,y,z] , x < y ]

addThree :: (Num a) => a->a->a->a
addThree x y z = x+y+z

mains =
  [
    do putStrLn (show "Hello World!")
    ,do putStrLn (show (foo 0))
    ,do putStrLn (show (foo 4))
    ,do putStrLn (show (foo 9223372036854775806))
    ,do putStrLn (show (foo 9223372036854775807))
    ,do putStrLn (show (bar 9223372036854775806))
    ,do putStrLn (show (bar 9223372036854775807))
    ,do putStrLn (show (zoi 9223372036854775806))
    ,do putStrLn (show (zoi 9223372036854775807))
    ,do putStrLn (show (zoi 9223372036854775806 == zoi 9223372036854775807))
    ,do putStrLn (show (tar 9223372036854775806))
    ,do putStrLn (show (tar 9223372036854775807))
    ,do putStrLn (show (tar 9223372036854775806 == tar 9223372036854775807))
    ,do putStrLn (show (yup (1,"tigrou",2)))
    ,do putStrLn (show (eqc 1 2))
    ,do putStrLn (show (ori 1 2 3 4))
    ,do putStrLn (show (read "True" || False))
    ,do putStrLn (show (read "True"::Bool))
    ,do putStrLn (show (read "\"True\""::String))
    ,do putStrLn (show (read "1"::Int))
    ,do putStrLn (show (succ LT))
    ,do putStrLn (show (minBound :: Bool))
    ,do putStrLn (show (addThree 1 2 3))
    ,do putStrLn (show ((fromIntegral (1::Int))+(1::Float)))
  ]

main :: IO ()
main = sequence_ mains
