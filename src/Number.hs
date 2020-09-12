{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Number (hexDigits, fromHex, binaryDigits, fromBinary, toHex) where
import Util (pad)
import Data.Char (intToDigit, toUpper)
import Numeric (showIntAtBase)
import Control.Arrow ((***))
import qualified Data.Map as Map
import Data.Kind (Constraint)
import Data.Tuple (swap)
import Data.Maybe (fromJust)
import Data.Coerce
import qualified Data.Map as Map
import GHC.TypeLits (Symbol)

enumerate :: Int -> [(Char, Int)]
enumerate n
  = tups xs ns
  where ns = take n ([0..9]<>[10..n])
        xs = take n (['0'..'9']<>['A'..'Z'])
        tups [] _ = []
        tups _ [] = []
        tups (x:xs) (y:ys)
          = (x,y):(tups xs ys)

class GEnum e where
  table :: [e]
  isDigit :: e -> Bool
  toDigit :: e -> Int
  valDigit :: Int -> e

data Hexadecimal :: * where
  Hexadecimal :: a -> Hexadecimal

instance Eq Hexadecimal
instance Ord Hexadecimal

instance GEnum Hexadecimal where
  table    = Hexadecimal <$> (take 16 (['0'..'9']<>['A'..'Z']))
  isDigit  = flip elem table
  valDigit = (!!) table
  toDigit  = fromJust . flip lookup (zip table [0..15])

instance Enum Hexadecimal where
  fromEnum = toDigit
  toEnum = valDigit

-- instance Num Hexadecimal where
--   fromInteger = convert ""
--     where
--       convert :: String -> Integer -> Hexadecimal
--       convert x  0 = Hexadecimal x
--       convert xs n
--         = convert x' n'
--         where
--           h = n `rem` 16
--           n' = n - ( i + h )
--           hs = toEnum (fromIntegral h)
--           x' = (:) hs xs

-- convert n []     = n
-- convert n (h:hs) = convert (16*n + h) hs

hexDigits :: [Char]
hexDigits = map fst $ enumerate 16

fromHexDigit :: Char -> Int
fromHexDigit = fromJust . flip lookup (enumerate 16)

fromHex :: [Char] -> Int
fromHex = convert 0
  where
    convert n []     = n
    convert n (h:hs) = convert (16*n + fromHexDigit(h)) hs

binaryDigits :: [Char]
binaryDigits = map fst $ enumerate 2

fromBinaryDigit :: Char -> Int
fromBinaryDigit = fromJust . flip lookup (enumerate 2)

fromBinary :: String -> Int
fromBinary = convert 0
  where
    convert n []     = n
    convert n (b:bs) = convert (2*n + fromBinaryDigit(b)) bs

toHex :: Int -> String
toHex h = toUpper <$> pad 2 '0' value
  where value = showIntAtBase 16 intToDigit h ""

toBinary :: Int -> String
toBinary b = value
  where value = showIntAtBase 2 intToDigit b ""
