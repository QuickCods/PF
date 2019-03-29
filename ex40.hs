import Data.Char

forte :: String -> Bool
forte s = (length s >= 8) && (cnt isLower /= 0) && (cnt isUpper /= 0) && (cnt isDigit /= 0)
        where
          cnt p = length (filter p s)
