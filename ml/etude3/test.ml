infixr 9 .
infixl 7 * / rem
infixl 6 + - ^
infixr 5 :
infixl 4 == <> <= >= < >
infixr 3 &&
infixr 2 ||
infixr 1 $

datatype (a) cons = Nil | Cons a * a cons
datatype expr =
  | EID string
  | EAPP expr * expr

#
# Convert () and [] to constants
# Convert (x) to x
# Convert && and || to if
# Define built-in functions

let main =
# case 1
# | (1,a) -> 3
case 1
| [1,2] -> 0
