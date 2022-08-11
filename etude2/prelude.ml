let + x y = x+y
let - x y = x-y
let * x y = x*y
let / x y = x/y
let rem x y = x rem y
let ^ x y = x^y
let : x y = x:y
let < x y = x<y
let > x y = x>y
let <= x y = x<=y
let >= x y = x>=y
let == x y = x==y
let <> x y = x<>y
let <- cons hd = cons <- hd
let hd cons = hd cons
let tl cons = tl cons
let ord c = ord c
let chr n = chr n
let size str = size str
let exit code = exit code

let pr x = print x; print "\n"; x

let dbg x = print "DEBUG: "; pr x

let identity x = x
let const k _ = k
let of f g x = f (g x)
let flip f x y = f y x

let forward x f = f x

let between low high x = low <= x && x < high
let negative x = 0 - x
let not x = if x then false else true

let foldl f x xs = let rec loop xs x =
                      if xs == [] then
                        x
                      else
                        loop (tl xs) (f x (hd xs))
                    in loop xs x
let foldl' f xs = foldl f (hd xs) (tl xs)
let reverse xs = foldl (fn x y-> y:x) [] xs
let foldr f x xs = foldl (flip f) x (reverse xs)
let foldr' f xs = foldl' (flip f) (reverse xs)
let append xs ys = foldr (:) ys xs
let flatten lists = foldr append [] lists
let map f xs = foldr (fn x y -> f x : y) [] xs
let flatmap f xs = flatten (map f xs)
let app f xs = foldl (fn _ y-> f y) [] xs
let rec any p xs = xs<>[] && (p (hd xs) || any p (tl xs))
let rec all p xs = xs==[] || (p (hd xs) && all p (tl xs))
let contains xs x = any (== x) xs
let doesnt_contain xs x = all (<> x) xs

let rec find pred xs =  if xs<>[] then
                          if pred (hd xs) then [hd xs]
                          else find pred (tl xs)
                        else []

let rec nth xs i = if i == 0 then hd xs else nth (tl xs) (i - 1)

let length xs = loop 0 xs
                where loop i xs = if xs==[] then i else loop (i+1) (tl xs)

let mapi f xs = loop 0 xs
                where loop i xs out = if xs == [] then reverse out else
                                      loop (i+1) (tl xs) (f i (hd xs) : out)


let implode strings = foldl (^) "" strings
let rec substr str i j =
  let low = 0 - size str in
  if between low 0 i then       substr str (i + size str) j
  else if between low 0 j then  substr str i (j + size str + 1)
  else                          loop (j - 1) []
                                where loop j out = if j >= i then
                                                     loop (j - 1) (str@j : out)
                                                   else
                                                     implode out

let unescape str = loop 0 []
                   where loop i out = if i < size str then
                                        if str @ i == "\\" then
                                          let c = str @ (i+1)
                                          and c' = if c == "n" then "\n" else
                                                   if c == "t" then "\t" else
                                                   c
                                          in loop (i+2) (c':out)
                                        else loop (i+1) (str@i:out)
                                      else implode (reverse out)
let escape quote str = loop 0 []
                       where loop i out = if i < size str then
                                            let c = str @ i
                                            and c' = if c == quote then "\\" ^ c else
                                                     if c == "\n" then "\\n" else
                                                     if c == "\t" then "\\t" else
                                                     if c == "\\" then "\\\\" else
                                                     c
                                            in loop (i+1) (c':out)
                                          else implode (reverse out)

let char_in set c = loop 0
                    where loop i = i < size set && (set @ i == c || loop (i+1))

let joinwith delim xs = if xs==[] then "" else
                        foldr' (fn x y -> implode [x, delim, y]) xs

let islower c = between 97 123 (ord c)
let isupper c = between 65 91 (ord c)
let isalpha c = islower c || isupper c
let isdigit c = between 48 58 (ord c)
let isalnum c = isalpha c || isdigit c
let isspace c = c==" " || c== "\t" || c=="\n"

let itoa n = if n==0 then "0" else
             if n<0 then "-" ^ loop (0 - n) [] else
             loop n []
             where loop n out = if n==0 then implode out
                                else loop (n/10) (chr (n rem 10+48):out)

let atoi str = if str @ 0 == "-" then 0 - (loop 1 0)
               else loop 0 0
               where loop i out = if i < size str && isdigit (str @ i) then
                                    loop (i+1) (out * 10 + ord (str @ i) - 48)
                                  else out
