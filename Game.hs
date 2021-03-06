module Game where

import Data.Array.IArray

data Symbol
  = Wall      -- ^ 0
  | Empty     -- ^ 1
  | Pill      -- ^ 2  
  | PowerPill -- ^ 3
  | Fruit     -- ^ 4
  | LambdaMan -- ^ 5
  | Ghost     -- ^ 6
  deriving (Eq, Ord, Show, Bounded, Enum)

symbolToChar ::  Symbol -> Char
symbolToChar Empty     = ' '
symbolToChar Wall      = '#'
symbolToChar Pill      = '.'
symbolToChar PowerPill = 'o'
symbolToChar Fruit     = '%'
symbolToChar LambdaMan = '\\'
symbolToChar Ghost     = '='

symbolFromChar :: Char -> Symbol
symbolFromChar ' ' = Empty
symbolFromChar '#' = Wall
symbolFromChar '.' = Pill
symbolFromChar 'o' = PowerPill
symbolFromChar '%' = Fruit
symbolFromChar '\\' = LambdaMan
symbolFromChar '=' = Ghost

type Map = Array (Int,Int) Symbol

type Level = Int

type Width = Int
type Height = Int

fruitPoint :: Level -> Integer
fruitPoint level
  | level > 12 = 5000
  | 1 <= level && level <= 12 = [100,300,500,500,700,700,1000,1000,2000,2000,3000,3000] !! level - 1 
  | otherwise = error "fruitPoint: unknown level"

computeLevel :: (Width,Height) -> Level
computeLevel (width,height) = ceiling $ fromIntegral (width * height) / (100::Rational)
-- computeLevel (15,18) == 3

-- 最大マップサイズは256*256なのでutcEOLはIntに32bit符号付き整数に収まる
type UTC = Int

utcEOL :: (Width,Height) -> UTC
utcEOL (w,h) = 127*w*h*16

utcFruit1Appear :: UTC
utcFruit1Appear = 127*200 

utcFruit2Appear :: UTC
utcFruit2Appear = 127*400

utcFruit1Expire :: UTC
utcFruit1Expire = 127*280

utcFruit2Expire :: UTC
utcFruit2Expire = 127*480

frightModeDuration :: UTC
frightModeDuration = 127*20

data Dir
  = DirUp    -- ^ 0
  | DirRight -- ^ 1
  | DirDown  -- ^ 2
  | DirLeft  -- ^ 3
  deriving (Eq, Ord, Show, Bounded, Enum)

oppositeDir :: Dir -> Dir
oppositeDir DirUp    = DirDown
oppositeDir DirDown  = DirUp
oppositeDir DirLeft  = DirRight
oppositeDir DirRight = DirLeft

data Vitality
  = Standard   -- ^ 0
  | FrightMode -- ^ 1
  | Invisible  -- ^ 2
  deriving (Eq, Ord, Show, Bounded, Enum)
