not x =
  case x
  | true = false
  | false = true;
and x y =
  case x
  | true = not (not y)
  | false = false;
or x y =
  case x
  | true = true
  | false = not (not y);
hd (x:y) = x;
tl (x:y) = y;
fst = hd;
snd = hd . tl;
plus x y = x + y;
minus x y = x - y;
times x y = x * y;
divide x y = x / y;
cons x y = x : y;
foldr f init list =
  case list
  | [] = init
  | (x:xs) = f x (foldr f init xs);

# > foldr cons [] [100,20,3];
> foldr plus 0 [100,20,3];
