inputFold :: (String -> a -> a) -> a -> IO a
inputFold fold seed = foldr fold seed . lines <$> readFile "input.txt"

main :: IO ()
main =
  let step l a =
        case l of
          ('+':ls) -> a + (read ls :: Integer)
          _ -> a + (read l :: Integer)
   in inputFold step 0 >>= print
