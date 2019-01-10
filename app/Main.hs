module Main where

import Data.List (groupBy)

main :: IO ()
--main = print (map (\ss -> foldl (\s t -> (fst t, snd s + snd t)) (0,0) ss) $ groupBy (\x y -> fst x == fst y) (winner 9 1000 23 0 1 []))
main = print (winner 9 1000 23 0 1 [])

winner :: Int -> Int -> Int -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
winner p n m s s' l =
  if m < n then
    if s'' < m then
      winner p n m s' s'' l
    else
      winner p n (m + 23) s' s'' ((mod m p, (s' - 6 + m)):l)
  else
    l
  where s'' = s' + (s' - s) * 2

