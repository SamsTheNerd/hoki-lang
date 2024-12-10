module Hoki.FrontUtil where


joinWith :: [a] -> [[a]] -> [a]
joinWith sep xs = if not $ null xs
                    then foldr1 (\a -> ((a ++ sep )++)) xs
                    else []
joinWith' :: [a] -> [[a]] -> [a]
joinWith' sep = joinWith sep . (++[[]])


trim :: String -> String
trim s = reverse tailless 
    where headless = dropWhile (`elem` " \n\r\t") s
          tailless = dropWhile (`elem` " \n\r\t") $ 
                        reverse headless
