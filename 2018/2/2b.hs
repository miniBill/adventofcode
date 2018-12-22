import Control.Monad (guard)
import qualified Data.List as List

a # b = b a

main :: IO ()
main =
  let diff a b = zipWith (/=) a b # filter id # length
   in do raw <- readFile "input.txt"
         let result = do
               l <- lines raw
               r <- lines raw
               guard $ diff l r == 1
               return (l, r, zipWith (\ll rl -> (ll == rl, ll)) l r # filter fst # map snd)
         print result
