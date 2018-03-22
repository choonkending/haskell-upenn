
toDigits :: Integer -> [Integer]
toDigits n =
  if n <= 0
    then []
    else toDigits (div n 10) ++ mod n 10 : []

toDigitsRev :: Integer -> [Integer]
toDigitsRev n =
  if n <= 0
    then []
    else mod n 10 : toDigitsRev (div n 10)

