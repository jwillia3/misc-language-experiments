; ex: ts=2:sw=2
(echo
(letrec
  (trace x)       (begin  (print "TRACE: ")
                          (echo x))
  (not x)         (if x false true)
  (not-equal? a b)(not (equal? a b))
  (pred n)        (sub n 1)
  (succ n)        (add n 1)
  (caar x)        (car (car x))
  (cdar x)        (cdr (car x))
  (cadr x)        (car (cdr x))
  (cddr x)        (cdr (cdr x))
  (first x)       (car x)
  (second x)      (cadr x)
  (third x)       (car (cddr x))
  (fourth x)      (cadr (cddr x))
  (o f g)         (lambda (x) (f (g x)))
  (nil? x)        (equal? (typeof x) 'nil)
  (bool? x)       (equal? (typeof x) 'bool)
  (int? x)        (equal? (typeof x) 'int)
  (sym? x)        (equal? (typeof x) 'sym)
  (str? x)        (equal? (typeof x) 'string)
  (cons? x)       (equal? (typeof x) 'cons)
  (fn? x)         (equal? (typeof x) 'fn)
  (spec? x)       (equal? (typeof x) 'spec)
  (between a c b) (and (>= b a) (<= b c))
  (digit? c)      (between (ord "0") (ord "9") (ord c))
  (lowercase? c)  (between (ord "a") (ord "z") (ord c))
  (uppercase? c)  (between (ord "A") (ord "Z") (ord c))
  (letter? c)     (or (lowercase? c) (uppercase? c))
  (alphanumeric? c) (or (letter? c) (digit? c))
  (space? c)      (elem '(9 10 13 32) (ord c))
  (foldl f init xs)
    (caseof xs
      ()        init
      (x . xs)  (foldl f (f init x) xs))
  (foldr f init xs)
    (caseof xs
      ()        init
      (x . xs)  (f x (foldr f init xs)))
  (count xs current)
    (caseof xs
      ()        current
      (_ . xs)  (count xs (succ current)))
  (reverse xs current)
    (caseof xs
      ()        current
      (x . xs)  (reverse xs (cons x current)))
  (flatten xs onto)
    (caseof xs
      ()        onto
      (x . xs)  (flatten x (flatten xs onto))
      _         (cons _ onto))
  (append xs ys)
    (foldr cons ys xs)
  (map f xs onto)
    (caseof xs
      ()      (reverse onto)
      (x.xs)  (map f xs (cons (f x) onto)))
  (filter keep xs onto)
    (caseof xs
      ()      (reverse onto)
      (x.xs)  (filter keep xs (if (keep x) (cons x onto) onto)))
  (elem set candidate)
    (caseof set
      ()    false
      (x.set) (or (equal? x candidate)
                  (elem set candidate)))
  
  ; LEXER/PARSER
  (iff ok?)
    (lambda (input)
      (caseof input
        ()        nil
        (x.input) (if (ok? x)
                      (cons [x] input)
                      ())))
  (lit item)
    (iff (lambda (given) (equal? item given)))
  (alt lhs rhs)
    (lambda (input)
      (caseof (lhs input)
        ()      (rhs input)
        result  result))
  (then lhs rhs)
    (lambda (input)
      (caseof (lhs input)
        ()        ()
        (a.input) (caseof (rhs input)
                    ()    ()
                    (b.input) (cons (append a b) input))))
  (star item)
    (lambda (input)
      (letrec
        (do input sofar)
          (caseof (item input)
            ()        (cons sofar input)
            (x.input) (do input (append x sofar)))
        (do input ())))
  (plus item)
    (then item (star item))
  (tag token-type result)
    (caseof result
      ()        ()
      (x.input) (cons (cons token-type (implode x)) input))
  (tokenise-all input)
    (letrec
      all-patterns
        [['int    (plus (iff digit?))]
         ['ide    (plus (iff alphanumeric?))]
         ['sp     (plus (iff space?))]]
    
      (get types input acc)
        (if input
          (caseof types
            ()                  ["out of options" (car input)]
            ((t match?).types)  (caseof (match? input)
                                  ()          (get types input acc)
                                  (txt.input) (get all-patterns
                                                   input
                                                   (cons
                                                     (cons t (implode txt))
                                                     acc))))
          acc)
      (reverse
        (filter (lambda (x) (not-equal? (car x) 'sp))
                (get all-patterns input nil))))

  (tokenise-all (explode (read-file "test.ml")))
))