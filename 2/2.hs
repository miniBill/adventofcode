import qualified Data.List as List

(#) = flip ($)

main :: IO ()
main =
  let
      hasTwo l = l # List.sort # List.group # filter (\l -> length l == 2) # null # not
      hasThree l = l # List.sort # List.group # filter (\l -> length l == 3) # null # not
   in
      do raw <- readFile "input.txt"
         let result =
               raw # lines # map (\l -> (hasTwo l, hasThree l)) # unzip #
               (\(w, r) -> length (filter id w) * length (filter id r))
         print result
