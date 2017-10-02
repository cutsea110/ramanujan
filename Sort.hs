module Sort (fsort) where

fsort :: ((Int, Int) -> Int) -> Int -> [(Int, Int)]
fsort r k = (k, k) : fmerge r [(k, b)|b<-[k+1..]] (fsort r (k+1))

fmerge :: (a -> Int) -> [a] -> [a] -> [a]
fmerge r (x:xs) (y:ys)
  | r x <= r y = x : fmerge r xs (y:ys)
  | otherwise  = y : fmerge r (x:xs) ys
