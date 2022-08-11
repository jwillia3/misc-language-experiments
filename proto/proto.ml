#
#   PRELUDE
#
let echo x = (pr x; pr "\n"; x)
and tr x = (pr "TRACE: "; echo x)
and trace x = tr x
and not x = if x then False else True
and && x y = if x then (if y then True else False) else False
and || x y = if x then True else if y then True else False
and == x y = _equal (x, y)
and <> x y = not (_equal (x, y))
and < x y = _less (x, y)
and <= x y = (|| (< x y) (== x y))
and > x y = not (<= x y)
and >= x y = not (< x y)
and + x y = _add (x, y)
and cons x y = (x, y)
and hd xs = case xs | (x:_) -> x
and tl xs = case xs | (_:x) -> x
and null x = case x | [] -> True | (_:_) -> False
and const k _ = k
and flip f x y = f y x
and combine f g x = f (g x)
and empty string = case string | "" -> True | (_^_) -> False

and foldl f init xs =
  let rec foldl' xs out =
    case xs
    | (x:xs') -> foldl' xs' (f out x)
    | [] -> out
  in foldl' xs init
and rec foldr f init xs =
  case xs
  | (x:xs') -> f x (foldr f init xs')
  | [] -> init
and reverse xs = foldl (flip cons) [] xs
and map f xs = foldr (\x xs-> f x : xs) [] xs
and foreach f xs = foldl (const f) () xs
and rec any accept xs =
  if null xs then
    False
  else if accept (hd xs) then
    True
  else
    any accept (tl xs)
and rec all accept xs =
  if null xs then
    True
  else if accept (hd xs) then
    all accept (tl xs)
  else
    False
and none accept xs = not (any accept xs)

and implode strings = foldl (\x y->x ^ y) "" strings

and islower c = (&& (<= (ord "a") (ord c)) (<= (ord c) (ord "z")))
and isupper c = (&& (<= (ord "A") (ord c)) (<= (ord c) (ord "Z")))
and isdigit c = (&& (<= (ord "0") (ord c)) (<= (ord c) (ord "9")))
and isalpha c = (|| (islower c) (isupper c))
and isalnum c = (|| (isalpha c) (isdigit c))
and hds s = case s | (x ^ _) -> x
and tls s = case s | (_ ^ x) -> x

in

()
