hx_show :: Show a => Int -> [a] -> String
hx_show _ [] = ""
hx_show 0 _ = "..."
hx_show _ [x] = show x
hx_show n (x:xs) = show x ++ ", " ++ hx_show (n-1) xs

hx_show :: Show a => Int -> a -> String
hx_show _ = show x
