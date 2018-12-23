import Control.Monad (ap, liftM)
import Data.List (group, groupBy, sort, sortBy)
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

parseLine :: SimpleParser (Int, Int, Int, Int, Int)
parseLine = do
  literal "#"
  claim <- number
  literal " @ "
  l <- number
  literal ","
  t <- number
  literal ": "
  w <- number
  literal "x"
  h <- number
  return (claim, l, t, w, h)

--(#) :: a -> (a -> b) -> b
a # b = b a

enumerate :: (Num a, Enum a) => a -> a -> a -> a -> [(a, a)]
enumerate l t w h = do
  y <- [t .. t + h - 1]
  x <- [l .. l + w - 1]
  return (x, y)

findHole [] = Nothing
findHole [x] = Nothing
findHole (x:y:zs)
  | y /= x + 1 = Just $ x + 1
  | otherwise = findHole (y : zs)

main :: IO ()
main =
  let points raw = do
        ls <- lines raw
        let (claim, l, t, w, h) =
              fromMaybe (0, 0, 0, 0, 0) $ evalParser parseLine ls
        (x, y) <- enumerate l t w h
        return (claim, x, y)
   in do raw <- readFile "input.txt"
         raw # points #
           sortBy (\(_, x, y) (_, x', y') -> compare (x, y) (x', y')) #
           groupBy (\(_, x, y) (_, x', y') -> (x, y) == (x', y')) #
           filter (\g -> length g > 1) #
           concatMap (map (\(c, _, _) -> c)) #
           sort #
           group #
           map head #
           findHole #
           print
