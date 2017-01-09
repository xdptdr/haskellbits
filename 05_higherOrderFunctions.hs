aaa :: (Num a, Ord a) => a -> a
aaa = max 6

bbb = flip (-)

mains =
  [
    do putStrLn (show "Hello World!")
    ,do putStrLn (show (aaa 8))
    ,do putStrLn (show (8 ` bbb` 6))
    ,do putStrLn (show (concat (map (\x -> x:x:[]) "Hello")))
    ,do putStrLn (show (filter (\x -> elem x "aeiouy") "Hello"))
    ,do putStrLn (show (filter (flip elem "aeiouy") "Hello"))
    ,do putStrLn (show (foldl (\acc x -> x:acc) "" "youpi"))
    ,do putStrLn (show (foldr (\x acc -> x:x:acc) "" "youpi"))
    ,do putStrLn (show (scanl (\acc x -> x:acc) "" "youpi"))
    ,do putStrLn (show (scanr (\x acc -> x:x:acc) "" "youpi"))
    ,do putStrLn (show $ abs.negate.(*3) $ 8)
  ]

main :: IO ()
main = sequence_ mains
