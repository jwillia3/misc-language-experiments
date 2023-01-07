# let

# and pr x = print x; print "\n"; x

# and foldl f init xs =
#     let (x:xs') = xs in pr xs'
#   # let rec go init xs =
#   #   # case pr xs
#   #   # | (x:xs') -> pr xs'; go (f init x) xs'
#   #   # | []      -> init
#   # in go init xs

# in
# foldl (fn x y -> x + y) 0 [100,20,3]

# let foldl xs =
#   # let (x:xs') = xs in 0
#   let (x:xs') = xs in 0
# in
# foldl [100,20,3]

# let xs = [1,2] in let (x:xs') = xs in 0

# THESE ARE FINE
# let xs = (1,2) in let (1,x) = xs in 0
# let xs = (1,2) in let (x,_) = xs in xs

# THESE ARE NOT
# let xs = (1,2) in let (x,2) = xs in xs
let xs = (1,2) in let (x,y) = xs in xs




# let xs = [1,2] in if iscons xs then let x = hd xs in let xs' = tl xs in 0 else 1000

# (let xs=[1, 2] in (if ((iscons xs)) then (let x=((hd xs)) in (let xs'=((tl xs)) in 0)) else (*CRASH*)))
# (let xs=[1, 2] in (if ((iscons xs)) then (let x=((hd xs)) in (let xs'=((tl xs)) in 0)) else 1000))
