import System.IO (isEOF)
import Text.Read (readMaybe)

inputFold :: Show a => (String -> a -> a) -> a -> IO ()
inputFold fold seed =
  let step i = do
        eof <- isEOF
        if eof
          then return i
          else do
            l <- getLine
            step (fold l i)
   in step seed >>= print

main :: IO ()
main =
  let step l a =
        case l of
          ('+':ls) -> a + (read ls :: Integer)
          _ -> a + (read l :: Integer)
   in inputFold step 0
