main :: IO ()
main = do
  raw <- readFile "input.txt"
  let result = sum $ map (read . filter (/= '+')) $ lines raw
  print result
