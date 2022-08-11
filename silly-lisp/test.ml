let lex filename src =
  # Return the first index that ok is not ture
  let rec while ok i =
    if i < strlen src && ok (char_at src i) then
      while ok (i + 1)
    else
      i
  in

  let err index msg = print ("ml: error " ^ msg ^ ".\n"); exit 1 in

  let idchr c = isalnum c || c == '_' || c == '\'' in
  let opchr c = char_in "!$%&*+-./:<=>?@^|~" c in
  let pun c = char_in "()[],;" c in
  let keywords = ["=","if","then","else","case","|","->","let",
                  "rec","and","in","fn","true","false","!"] in
  let idtype txt = if exists (== txt) keywords then touppers txt else "ID" in

  let character i =
    if char_at src i == '\\' then
      case char_at src (i + 1)
      | 'n' -> ('\n', i + 2)
      | 't' -> ('\t', i + 2)
      | c   -> (c, i + 2)
    else
      (char_at src i, i + 1)
  in

  # Read a single token from the string index given
  # Return: (type_string, token_text, next_index)
  let single i =
    let c = char_at src i in

    if isdigit c || c == '-' && (isdigit $ char_at src (i + 1)) then
      let j = while isdigit (i + 1) in
      ("INT", substring i j src, j)

    else if c == '\'' then
      let (txt, j) = character (i + 1) in
      if char_at src j <> '\'' then
        err j "unclosed char"
      else
        ("CHAR", txt, j + 1)

    else if c == '"' then
      let rec do i out =
        if i == strlen src || char_at src i == '\n' then
          err i "unclosed string"
        else if char_at src i == '"' then
          ("STRING", implode $ reverse out, i + 1)
        else
          let (c, i') = character i in
          do i' (c:out)
      in do (i + 1) []

    else if idchr c then
      let j = while idchr i in
      let id = substring i j src in
      (idtype id, id, j)

    else if opchr c then
      let j = while opchr i in
      let id = substring i j src in
      (idtype id, id, j)

    else if pun c then
      let txt = substring i (i + 1) src in
      (txt, txt, i + 1)

    else
      err i ("bad token: " ^ (char_to_string c))
  in

  let rec space i ln =
    if i < strlen src then
      case char_at src i
      | ' '  -> space (i + 1) ln
      | '\n' -> space (i + 1) (ln + 1)
      | '#'  -> space (while (<> '\n') (i + 1)) ln
      | _    -> (i, ln)
    else (i, ln)
  in

  # Read all tokens.
  # Return: [(location, type, text)]
  let rec all i line out =
    let (i, ln) = space i line in
    let loc = filename ^ ":" ^ itoa ln in
    if i < strlen src then
      let (type, text, j) = single i in
      let token = (loc, type, text) in
      all j ln (token:out)
    else
      reverse out
  in

  all 0 1 []

let filename = "prelude.ml"
let src = readfile filename
let tokens = lex filename src
let _ = app pr tokens
