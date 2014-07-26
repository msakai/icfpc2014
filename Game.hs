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

type UTC = Integer

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
