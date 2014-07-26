module Game where

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

