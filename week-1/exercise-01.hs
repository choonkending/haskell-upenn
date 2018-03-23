
toDigits :: Integer -> [Integer]
toDigits n =
  if n <= 0
    then []
    else toDigits (div n 10) ++ [mod n 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n =
  if n <= 0
    then []
    else mod n 10 : toDigitsRev (div n 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther l =
  case l of
    (h : t) ->
      if (mod (length l) 2 == 0)
        then h * 2 : doubleEveryOther t
        else h : doubleEveryOther t
    [] -> []


sumDigits ::  [Integer] -> Integer
-- sumDigits = foldr (\a b -> foldr (+) 0 (toDigits a) + b) 0
-- sumDigits = foldr ((+) . (foldr (+) 0) . toDigits) 0
sumDigits = foldr ((+) . sum . toDigits) 0

validate :: Integer -> Bool
validate = (== 0) . (flip mod 10) . sumDigits . doubleEveryOther . toDigits


-- Given the number of discs and names for the three pegs,
-- hanoi should return a list of moves to be performed to
-- move the stack of discss from the first peg to the second.
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n == 1 = [(a, b)]
  | otherwise = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a

