module Util (checksum, padL, sum, pad, swap, padR) where
import Prelude hiding (sum)
import Data.Bits

checksum :: [[Int]] -> Int
checksum a  = c
  where  s  = sum a
         c  = 0x100 - (s .&. 0xFF)

sum1 :: [Int] -> Int
sum1 = foldl (+) 0
sum :: [[Int]] -> Int
sum x = sum1 $ sum1 <$> x

pad :: Int -> Char -> String -> String
pad n c x = replicate (n - length x) c ++ x
padL :: Int -> Char -> String -> String
padL = pad
padR :: Int -> Char -> String -> String
padR n c x = x ++ replicate (n - length x) c

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)
