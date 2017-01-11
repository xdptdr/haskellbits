data FBZ x = Foo x | Bar x x | Zoi x x x | Void deriving (Show)

instance Functor FBZ where
  fmap f (Foo x) = Foo (f x)
  fmap f (Bar x y) = Bar (f x) (f y)
  fmap f (Zoi x y z) = Zoi (f x) (f y) (f z)

instance Applicative FBZ where
  pure = Foo
  (<*>) (Foo f) (Foo x) = Foo (f x)
  (<*>) (Bar f g) (Foo x) = Bar (f x) (g x)
  (<*>) (Zoi f g h) (Foo x) = Zoi (f x) (g x) (h x)
  (<*>) (Foo f) (Bar x y) = Bar (f x) (f y)
  (<*>) (Bar f g) (Bar x y) = Bar (f x) (g y)
  (<*>) (Zoi f g h) (Bar x y) = Zoi (f x) (g y) (h y)
  (<*>) (Foo f) (Zoi x y z) = Zoi (f x) (f y) (f y)
  (<*>) (Bar f g) (Zoi x y z) = Zoi (f x) (g y) (g y)
  (<*>) (Zoi f g h) (Zoi x y z) = Zoi (f x) (g y) (h y)

instance Monad FBZ where
  return = Foo
  fail _ = Void
  (>>=) (Foo x) f = f x
  (>>=) (Bar x y) f = f x
  (>>=) (Zoi x y z) f = f x

foo :: Int -> FBZ Int
foo x
  | x<10 = Zoi (x-30) (x-20) x
  | x<20 = Bar (x-10) x
  | x<30 = Foo (x+1)
  | otherwise = Zoi x x x

mains =
  [
    do putStrLn.show $ "Hello World!"
    ,do putStrLn.show $ (Zoi 3 4 5)
    ,do putStrLn.show $ fmap (\x->x+8)(Zoi 3 4 5)
    ,do putStrLn.show $ ((pure 4)::(FBZ Int))
    ,do putStrLn.show $ ((return 5)::(FBZ Int))
    ,do putStrLn.show $ ((pure (\x -> x++(reverse x))) <*> (pure "hello")::(FBZ [Char]))
    ,do putStrLn.show $ ((pure (\x -> x++(reverse x))) <*> (Bar "hello" "lol")::(FBZ [Char]))
    ,do putStrLn.show $ ((Bar (\x -> x++(reverse x)) (\x -> x++x)) <*> (Bar "hello" "lola")::(FBZ [Char]))
    ,do putStrLn.show $ Foo 10 >>= foo >>= foo >>= foo >>= foo
    ,do putStrLn.show $ Foo 40 >>= foo
  ]

main :: IO ()
main = sequence_ mains
