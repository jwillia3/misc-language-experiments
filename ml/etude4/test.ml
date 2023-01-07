infixr 0 $
infixl 2 ||
infixl 3 &&
infixl 4 == <> <= >= < >
infixr 5 :
infixl 6 + - ^
infixl 7 * /
infixl 9 .

# datatype boole = FALSE | TRUE
# datatype (a) list = NIL | CONS a * a list
datatype (a) map = EMPTY_MAP | MAP string * a * a map


# let x = fn x -> (1,2)
#         |  x -> x
#         # |  () -> 0

# let f x =
#   case x
#   | a -> 10
#   | "" -> 2

# let f [x,y] = y

# let f = +
# let f (a : b) = (a, b)
# and g x = 10

let x = 1
and y = 2
and 3 = 2
and z = 5

# let
#     f x y z = 1
#   | f a b c = 2
# and g x = 2

# let f x = 1
#   | f y = 2

# let _ =
#   case 1
#   | x -> 2
#   # | 3 -> 4
#   # | 5 -> 6
