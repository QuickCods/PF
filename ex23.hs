-- a)

aprox :: Int -> Double
aprox n = 4 * (sum [(-1)^n / (2 * fromIntegral n + 1) | n <- [0..n]])

-- b)

aprox' :: Int -> Double
aprox' k = sqrt(12 * sum [(-1)^k / ((fromIntegral k + 1)^2) | k <- [0..k]])

