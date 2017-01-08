aaa :: (Integral a) => a -> String
aaa 7 = "good"
aaa _ = "bad"

bbb :: [Char] -> (Char, Int)
bbb "" = ('0', 0)
bbb str@(a:b) = (a, length str)

ccc :: (Integral a) => a -> String
ccc x
  | x `mod` 2 == 0 = "even"
  | otherwise = "odd"

ddd :: [Char] -> String
ddd x
  | len `mod` 2 == 0 = "even length"
  | otherwise = "odd length"
  where len = (length x)

eee :: (Integral a) => a -> String
eee x = case x of 1 -> "unit" ; otherwise -> "nonunit"

mains =
  [
    do putStrLn (show "Hello World!")
    ,do putStrLn (aaa 0)
    ,do putStrLn (aaa 7)
    ,do putStrLn (show (bbb ""))
    ,do putStrLn (show (bbb "Hello World!"))
    ,do putStrLn (show (ccc 5))
    ,do putStrLn (show (ccc 6))
    ,do putStrLn (show (ddd "hello"))
    ,do putStrLn (show (ddd "hello me"))
    ,do putStrLn (show (let a=3 ; b=7 in (a,b,a,b)))
    ,do putStrLn (show (eee 1))
    ,do putStrLn (show (eee 2))
  ]

main :: IO ()
main = sequence_ mains
