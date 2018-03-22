
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


