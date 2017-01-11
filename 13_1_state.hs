import Control.Monad.State

data ComboList = ComboList {
  items :: [String]
  ,selected :: Maybe String
} deriving (Show)

data Button = Button {
  text :: String,
  active :: Bool
} deriving (Show)

select' :: Maybe String -> ComboList -> ((), ComboList)
select' x c = ((), ComboList {items = items c, selected = x})

select :: Maybe String -> State ComboList ()
select x = state $ select' x

getSize' :: ComboList -> (Int, ComboList)
getSize' c = (length.items $ c, c)

getSize :: State ComboList Int
getSize = state $ getSize'

mains =
  [
    do putStrLn.show $ "Hello World!"
  ]

main :: IO ()
main = sequence_ mains
