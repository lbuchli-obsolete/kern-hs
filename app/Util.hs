module Util where

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, x, _) = x

third :: (a, b, c) -> c
third (_, _, x) = x

mapLeft :: (a0 -> a) -> Either a0 b -> Either a b
mapLeft f (Left a0) = Left (f a0)
mapLeft _ (Right b) = Right b
