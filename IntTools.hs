module IntTools where

ec :: (Enum a, Enum b) => a -> b
ec = toEnum . fromEnum

toUnsigned :: Int -> Integer -> Integer
toUnsigned bits val =
  if val < 0
     then val + 2 ^ bits
     else val

toSigned :: Int -> Integer -> Integer
toSigned bits val =
  if val >= 2 ^ (bits - 1)
     then val - 2 ^ bits
     else val

toUnsigned' :: (Enum a, Enum b) => Int -> a -> b
toUnsigned' bits = ec . toUnsigned bits . ec

toSigned' :: (Enum a, Enum b) => Int -> a -> b
toSigned' bits = ec . toSigned bits . ec

changeBits :: (Enum a, Enum b) => Int -> Int -> a -> b
changeBits from to =
  ec . toSigned to . (`mod` 2 ^ to) .
  toUnsigned from . ec
