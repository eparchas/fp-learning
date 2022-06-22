module Lib
    ( maxProfit
    ) where

{- | To calculate the maximum possible profit with multiple trades and a cost for selling 
     we follow a strategy of buying at the local minimum and selling at the local maximum
     only when making a profit is possible. We thus calculate the local minima and maxima
     of the function (critical points) and then go through them, buyin and selling when it
     is possible to make a profit -}
maxProfit :: Int -> [Int] -> Int
maxProfit fee prices = result where
    cs = criticalPoints prices
    result = maxProfitableSum cs fee Nothing 0

{- | Calculate the maximum profit based on the critical points and the cost of trading
maxProfitableSum :: Critical Points -> Trade Cost -> Trade Open -> Running Profit -> Result -}
maxProfitableSum :: [Int] -> Int -> Maybe Int -> Int -> Int
maxProfitableSum [] _ _ s = s
maxProfitableSum (h:t) c Nothing s = maxProfitableSum t c (Just h) s
maxProfitableSum (h:t) c (Just to) s
    | h <= to    = maxProfitableSum t c (Just h) s
    | h > to + c = maxProfitableSum t c Nothing (s + h - to - c)
    | otherwise  = maxProfitableSum t c (Just to) s

-- | Find the critical points of the function
criticalPoints :: [Int] -> [Int]
criticalPoints []     = []
criticalPoints [a]    = [a]
criticalPoints [a, b] = [a, b]
criticalPoints ps     = cp where
    eps = head ps : ps ++ [last ps] -- would be nice to have a more efficient way to acess the last element
    dxs = zip3 eps (drop 1 eps) (drop 2 eps)
    cp  = map (\(_, c, _) -> c) $ filter (\(p, c, n) -> signum (p - c) /= signum (c - n)) dxs