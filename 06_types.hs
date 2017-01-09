data BiBam = Bim | Bam

data Shape a = Circle a | Rectangle a a deriving (Show)

surface :: Shape Int -> Double
surface (Circle r) = pi * (fromIntegral r) ^ 2
surface (Rectangle w h) = fromIntegral $ w * h

data Person = Person { firstname :: String, lastname :: String } deriving (Show)

initials :: Person -> String
initials p = let (f,l) = (head.firstname $ p, head.lastname $ p) in f : ". " ++ l : "."

class Fooable a where
  foo :: a -> Char

instance Fooable Int where
  foo x = 'a'

instance Fooable Integer where
  foo x = 'b'

bimfoo :: (Fooable a) => a -> String
bimfoo x = (foo x):(foo x):""

data Youpi x = Titi x | Tata x deriving (Show)

instance Functor Youpi where
	fmap f (Titi a) = (Tata (f a))
	fmap f (Tata a) = (Titi (f a))

mains =
  [
    do putStrLn.show $ "Hello World!"
    ,do putStrLn.show $ surface $ Rectangle 5 6
    ,do putStrLn.show $ Rectangle 5 6
    ,do putStrLn.show $ map (surface.Circle) [1..5]
    ,do putStrLn.show $ map (surface.Circle) [1..5]
    ,do putStrLn.show $ Person {firstname="Me", lastname="Too"}
    ,do putStrLn.show $ firstname $ Person {firstname="Me", lastname="Too"}
    ,do putStrLn.show $ initials $ Person {firstname="Me", lastname="Too"}
    ,do putStrLn.show $ bimfoo (5::Int)
    ,do putStrLn.show $ bimfoo (5::Integer)
    ,do putStrLn.show $ fmap (\x -> x)(Titi 4)
    ,do putStrLn.show $ fmap (\x -> length x)(Tata "hello")
  ]

main :: IO ()
main = sequence_ mains
