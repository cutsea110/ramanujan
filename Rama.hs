module Rama (ramanujan) where

import Sort (fsort)

ramanujan :: [((Int, Int), (Int, Int))]
ramanujan = [(x,y)|(x,y)<-zip s (tail s), sumcubes x == sumcubes y]
  where
    s = fsort sumcubes 1

sumcubes :: (Int, Int) -> Int
sumcubes (a,b) = a^3+b^3
