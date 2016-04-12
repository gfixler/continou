module Image where

import Color

type Image a = (Double, Double) -> a

always :: a -> Image a
always = const

leftAndRight :: a -> a -> Image a
leftAndRight l r = \(x,y) -> if x < 0 then l else r

lerp :: (Enum a, Fractional a) => a -> a -> Int -> [a]
lerp s e n = [(e - s) / n' * i + s | i <- [0..n']]
    where n' = fromIntegral (n - 1)

-- lerp property-based tests

-- length (lerp 5 (-3) 13)

-- let xs = lerp 1 5 5 in zip (tail xs) xs

-- let xs = lerp (-3.461) 5.1235 17 in zipWith (-) (tail xs) xs

-- let ys = let xs = lerp (-3.461) 5.1235 17
--       in zipWith (-) (tail xs) xs in zip ys (tail ys)

-- let ys = let xs = lerp (-3.461) 5.1235 17
--       in zipWith (-) (tail xs) xs in zipWith (-) ys (tail ys)

-- map (<0.0001) $ let ys = let xs = lerp (-3.461) 5.1235 17 in zipWith (-)
-- (tail xs) xs in zipWith (-) ys (tail ys)

-- map (<0.0000000000000001) $ let ys = let xs = lerp (-3.461) 5.1235 17 in
-- zipWith (-) (tail xs) xs in zipWith (-) ys (tail ys)

-- map (< 10^^(-5)) $ let ys = let xs = lerp (-3.461) 5.1235 17 in zipWith
-- (-) (tail xs) xs in zipWith (-) ys (tail ys)

