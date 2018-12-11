import Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main =
  let go found (e:es) =
        if e `Set.member` found
          then e
          else go (Set.insert e found) es
   in do raw <- readFile "input.txt"
         let repeating = cycle $ map (read . filter (/= '+')) $ lines raw
         let psums = scanl (+) 0 repeating
         print $ go Set.empty psums
