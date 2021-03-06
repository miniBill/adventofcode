import Control.Monad (ap, liftM)
import Data.List (group, sort)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

newtype SimpleParser a = SimpleParser
  { runParser :: String -> Maybe (a, String)
  }

evalParser :: SimpleParser a -> String -> Maybe a
evalParser p = fmap fst . runParser p

mmap :: (a -> Maybe b) -> SimpleParser a -> SimpleParser b
mmap f (SimpleParser p) =
  SimpleParser $ \s -> do
    (x, r) <- p s
    y <- f x
    return (y, r)

oneOf :: String -> SimpleParser Char
oneOf = SimpleParser . oneOf'
  where
    oneOf' _ [] = Nothing
    oneOf' [] _ = Nothing
    oneOf' (l:ls) (s:ss)
      | l == s = Just (s, ss)
      | otherwise = oneOf' ls (s : ss)

many :: SimpleParser a -> SimpleParser [a]
many p =
  SimpleParser $ \s ->
    let try = do
          h <- p
          t <- many p
          return (h : t)
     in runParser try s # fromMaybe ([], s) # Just

number :: SimpleParser Int
number =
  let digit = oneOf ['0' .. '9']
   in mmap readMaybe $ many digit

literal :: String -> SimpleParser ()
literal =
  let seed = return ()
      step x = (oneOf [x] >>)
   in foldr step seed

instance Functor SimpleParser where
  fmap = liftM

instance Applicative SimpleParser where
  pure x = SimpleParser $ \s -> Just (x, s)
  (<*>) = ap

instance Monad SimpleParser where
  (SimpleParser x) >>= f =
    SimpleParser $ \s -> do
      (x', s') <- x s
      runParser (f x') s'

parseLine :: SimpleParser (Int, Int, Int, Int)
parseLine = do
  literal "#"
  _ <- number
  literal " @ "
  l <- number
  literal ","
  t <- number
  literal ": "
  w <- number
  literal "x"
  h <- number
  return (l, t, w, h)

(#) :: a -> (a -> b) -> b
a # b = b a

enumerate :: (Num a, Enum a) => a -> a -> a -> a -> [(a, a)]
enumerate l t w h = do
  y <- [t .. t + h - 1]
  x <- [l .. l + w - 1]
  return (x, y)

main :: IO ()
main =
  let points raw = do
        ls <- lines raw
        let (l, t, w, h) = fromMaybe (0, 0, 0, 0) $ evalParser parseLine ls
        enumerate l t w h
   in do raw <- readFile "input.txt"
         raw # points # sort # group # map (\g -> (head g, length g)) #
           filter (\(_, l) -> l > 1) #length# print
