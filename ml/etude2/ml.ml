let resv = ["=","let","rec","and","in","where","fn","->","if",
              "then","else","true","false"]

let lex fname src = map finalise (all 1 0 [])
  where
  and isidchr c = isalnum c || c=="_" || c=="'"
  and isopchr c = char_in "!$%&*+-./:<=>?@^|~" c
  and ispun c = char_in "()[],;`" c
  and while p k i = if i < size src && p (src @ i) then
                      while p k (i + 1)
                    else
                      k i
  and char i = let j = if src@i == "\\" then i+2 else i+1 in
               if src@j <> "'" then [j, "error", "unclosed char"] else
               [j+1, ["char", substr src i j]]
  and string i =  let rec loop j =
                    if j >= size src then [j, ["error", "unclosed string"]] else
                    if src@j == "\"" then [j+1, ["string", substr src i j]] else
                    if src@j == "\\" then loop (j+2) else
                    loop (j+1)
                  in loop i

  and single i =  let c = src @ i in
                  let finish type j = [j, [type, substr src i j]] in
                  if c=="-" && isdigit (src@(i+1)) then
                                    while isdigit (finish "int") (i+1) else
                  if isdigit c then while isdigit (finish "int") (i+1) else
                  if ispun c then   [i+1, [c, c]] else
                  if c=="'" then    char (i+1) else
                  if c=="\"" then   string (i+1) else
                  if isidchr c then while isidchr (finish "id") (i+1) else
                  if isopchr c then while isopchr (finish "id") (i+1) else
                  [i + 1, "error", "bad token: " ^ c]

  and all ln i out =  let c = if i < size src then src @ i else "" in
                      if c == " "  then all ln (i+1) out else
                      if c == "\n" then all (ln+1) (i+1) out else
                      if c == "#"  then all ln (while (<> "\n") identity i) out else
                      let loc = implode [fname, ":", itoa ln] in
                      if c == "" then reverse ([loc, "end", ""]:out) else
                      let [j, tok] = single i
                      in all ln j ((loc:tok):out)

  and finalise token = let [loc, type, text] = token in
                       if type=="id" then if contains resv text then
                                            [loc, text, text]
                                          else
                                            [loc, type, text]
                       else if type=="char" then [loc, type, unescape text]
                       else if type=="string" then [loc, type, unescape text]
                       else if type=="error" then pr $ implode ["ml: error ",
                                                                loc, ": ",
                                                                text];
                                                  exit 1
                       else [loc, type, text]

#
# EXPRESSION CONSTRUCTORS
#

let EINT loc x = [loc, "int", x]
let ECHAR loc x = [loc, "char", x]
let ESTRING loc x = [loc, "string", x]
let EVAR loc id = [loc, "id", id]
let ETUPLE loc xs = [loc, "tuple", xs]
let ELIST loc xs = [loc, "list", xs]
let EFN loc param body = [loc, "fn", param, body]
let EAPP f op x = [hd f, "app", f, op, x]
let ELET loc decls body = [loc, "let", decls, body]
let ELETREC loc decls body = [loc, "letrec", decls, body]
let EIF loc a b c = [loc, "if", a, b, c]

let parse src =

  let
  and feed = [src]
  and current = [[]]
  and tloc  _ = hd current nth 0
  and ttype _ = hd current nth 1
  and ttext _ = hd current nth 2

  and peek _ =  if hd feed <> [] then hd (hd feed) else []
  and next _ =  if hd feed <> [] then
                  let token = hd (hd feed) in
                  current <- token;
                  feed <- tl (hd feed);
                  true
                else false
  and want t =  if peek () nth 1 == t then next () else false
  and syntax msg = next(); # Move to next token for error location
                   pr $ implode ["ml: error ", tloc(), ": ", msg];
                   exit 1
  and need t =  if want t then () else syntax ("need " ^ t)
  and locof x = hd x
  and infixes = [["*",8,9],["/",8,9],["rem",8,9],
                 ["+",7,8],["-",7,8],["@",7,8],["^",7,7],
                 ["nth",6,7],[":",6,6],
                 ["==",5,6],["<>",5,6],["<=",5,6],[">=",5,6],
                 ["<",5,6],[">",5,6],
                 ["&&",4,4],
                 ["||",3,3],
                 ["<-",2,3],
                 ["$",1,1],
                 [";",0,0]]
  and findop _  = let [loc, type, text] = peek() in
                  if type == "id" then
                    find (fn x -> hd x == text) infixes
                  else []
  in

  let rec

  and expr _ = iexpr 0

  and iexpr level =
    if level > 10 then
      if want "if" then let loc = tloc()
                        and a = expr ()
                        and b = need "then"; expr ()
                        and c = need "else"; expr ()
                        in EIF loc a b c
      else
      if want "let" then  let loc = tloc()
                          and isrec = want "rec"
                          and decls = want "and"; decls ()
                          and body  = need "in"; expr()
                          in
                          if isrec then ELETREC loc decls body
                          else ELET loc decls body
      else

      # Function application
      let rec loop lhs =  let rhs = aexpr false in
                          if rhs<>[] then loop (EAPP lhs "$" rhs)
                          else lhs
      in loop (aexpr true)
    else
      let rec loop lhs =
        let op = findop () in
        let [id, llevel, rlevel] = if op<>[] then hd op else ["",-1,-1] in
        if llevel == level then
          next(); # Eat operator
          let rhs = iexpr rlevel in
          loop (EAPP lhs id rhs)
        else lhs
      in loop (iexpr (level + 1))

  and aexpr required =
    if not required && findop() <> [] then [] else
    if want "int" then    EINT    (tloc()) (atoi $ ttext()) else
    if want "char" then   ECHAR   (tloc()) (ttext()) else
    if want "string" then ESTRING (tloc()) (ttext()) else
    if want "id" then     EVAR    (tloc()) (ttext()) else
    if want "(" then      ETUPLE  (tloc()) (listof expr ")") else
    if want "[" then      ELIST   (tloc()) (listof expr "]") else
    if want "fn" then     fnexpr "->" else
    if required then      syntax "need expression" else
    []

  and fnexpr delim =
    if want delim then
      let body = expr ()
      and decls = if delim=="=" && want "where" then decls () else []
      in
      if decls <> [] then
        ELETREC (locof body) decls body
      else
        body
    else
      let loc = tloc()
      and param = (need "id"; ttext())
      and body = fnexpr delim
      in [loc, "fn", param, body]

  and decls _ =
    let
    and simple _ =  let id = need "id"; ttext()
                    and rhs = fnexpr "="
                    in [[id, rhs]]
    and destruct _ =  let loc = tloc()
                      and ids = listof (fn _ -> need "id"; ttext()) "]"
                      and rhs = need "="; expr ()
                      and it = "__"
                      and first = [it, rhs]
                      and fetch i x = let rhs = (EAPP (EVAR loc it)
                                                      "nth"
                                                      (EINT loc i))
                                      in [x, rhs]
                      and rest = mapi fetch ids
                      in first:rest
    in
    let these = if want "[" then destruct () else simple ()
    and those = if want "and" then decls () else []
    in append these those

  and listof item delim =
    if want delim then [] else
    let x = item ()
    and xs = if want "," then listof item delim else (need delim; [])
    in x:xs

  and top out =
    if want "end" then
      reverse out
    else
      need "let";
      let isrec = want "rec"
      and decls = decls ()
      in top (decls:out)
  in
  top []

let rec pp e =
  let [loc, type] = e
  and tmp = tl (tl e)
  and len = length tmp
  and a = if len > 0 then tmp nth 0 else ()
  and b = if len > 1 then tmp nth 1 else ()
  and c = if len > 2 then tmp nth 2 else ()
  in
  if type == "int" then let [x] = tmp in itoa x else
  if type == "char" then let [x] = tmp in "'" ^ escape "'" x ^ "'" else
  if type == "string" then let [x] = tmp in "\"" ^ escape "\"" x ^ "\"" else
  if type == "id" then let [x]=tmp in x else
  if type == "tuple" then "(" ^ joinwith "," (map pp a) ^ ")" else
  if type == "list" then "[" ^ joinwith "," (map pp a) ^ "]" else
  if type == "fn" then "fn " ^ a ^ " -> " ^ pp b else
  if type == "let" then "let " ^ joinwith " and " (map pp_decl a) ^
                        " in " ^ pp b else
  if type == "letrec" then "let rec " ^ joinwith " and " (map pp_decl a) ^
                        " in " ^ pp b else
  if type == "if" then "if " ^ pp a ^ " then " ^ pp b ^ " else " ^ pp c else
  if type == "app" then pp a ^ " " ^ b ^ " " ^ pp c else
  "NOT_PRINTED"

  where pp_decl decl = let [id, value] = decl in id ^ "=" ^ pp value

let fname = "test.ml"
# let fname = "prelude.ml"
let src = readfile fname
let tokens = lex fname src
let decls = parse tokens

let _ = app pr (map pp_set decls)
        where pp_set decls = "let " ^ joinwith "\nand " (map pp_decl decls)
        and   pp_decl decl = implode [hd decl, " = ", pp (decl nth 1)]
