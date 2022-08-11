(define no-pos "unknown:0")

;
;   Expressions
;
(define (expr-loc e)            (first e))
(define (expr-type e)           (second e))
(define (expr-etc e)            (third e))

(define (enil loc)              (list loc 'nil))
(define (eunit loc)             (list loc 'unit))
(define (eint loc int)          (list loc 'int int))
(define (echr loc chr)          (list loc 'chr chr))
(define (estr loc str)          (list loc 'str str))
(define (evar loc id)           (list loc 'var id))
(define (ectr loc id)           (list loc 'ctr id))
(define (etup loc xs)           (list loc 'tup xs))
(define (econs x y)             (list (car x) 'cons x y))
(define (eapp x y)              (list (car x) 'app x y))
(define (eif loc a b c)         (list loc 'if a b c))
(define (ecase loc sub rules)   (list loc 'case sub rules))
(define (elet loc id val body)  (list loc 'let id val body))
(define (erec loc defs body)    (list loc 'rec defs body))
(define (efn loc param body)    (list loc 'fn param body))

(define no-expr (eunit no-pos))

(define (append-to-let e new)
        (case e
              (_ 'let _ _ ())     (set-nth e 4 new)
              (_ 'let _ _ e)      (append-to-let e new)
              (_ 'rec _ ())       (set-nth e 3 new)
              (_ 'rec _ e)        (append-to-let e new)
              (_ 'case _ (rule))  (if rule
                                      (append-to-let (second rule) new)
                                      (set-nth rule 2 new)))
        e)

;
;   Types
;   All types have the same form (form id inst args)
;
(define (typevar id)          (list 'var    id  ()  ()))
(define (basetype id . args)  (list 'base   id  ()  args))
(define (tuple-type args)     (list 'tuple  ""  ()  args))
(define (fn-type lhs rhs)     (list 'fn     ""  ()  (list lhs rhs)))

(define (type-form t)         (first t))
(define (type-id t)           (second t))
(define (type-inst t)         (third t))
(define (type-args t)         (fourth t))

(define (fn-type? t)          (equal? 'fn (type-form t)))
(define (typevar? t)          (equal? 'var (type-form t)))

(define (set-type-inst v to)  (if (typevar? v) (setcar (cddr t) to) to))

(define (prune t)             (if (type-inst t) (prune (type-inst t)) t))
(define (occurs-in? v type)   (define type (prune type))
                              (or (equal? v type)
                                  (any? (partial-apply equal? v)
                                        (type-args type))))
(define (unifies? t u)
        (define t (prune t))
        (define u (prune u))
        (cond (typevar? t)    (if (occurs-in? t)
                                  (equal? t u)
                                  (set-type-inst t u))
              (typevar? u)    (unifies? u t)
              'else           (and (equal? (type-form t) (type-form u))
                                   (equal? (type-id t) (type-id u))
                                   (equal? (length (type-args t)) (length (type-args u)))
                                   (all? (unpair unifies?)
                                         (zip (type-args t) (type-args u))))))

(define uid-cnt 0)
(define (uid) (implodes "$" (integer->string (set uid-cnt (+ 1 uid-cnt)))))

; Read all tokens: ((loc type text))
(define (lex fn src)
  (define resv (map symbol->string '(let rec datatype = and | if
                                     then else case | -> in fn infixl infixr)))
  (define (tag tag i pred)  (define j (character-span pred src (+ 1 i)))
                            (define text (substring src i j))
                            (list j tag text))
  (define (not-eol? c)  (not-equal? c "\n"))
  (define (pun? c)      (find-string "()[],;" c))
  (define (id-char? c)  (or (alnum? c) (equal? c "_") (equal? c "'")))
  (define (op-char? c)  (find-string "!$%&*+-./:<=>?@^|~" c))
  (define (tail want tag i j) (if (equal? want (char-at src j))
                                  (list (+ 1 j) tag (substring src (+ 1 i) j))
                                  (list j "error" (implodes tag " not closed"))))

  (define (char-lit i)    (define j (if (equal? "\\" (char-at src (+ 1 i)))
                                        (+ 3 i)
                                        (+ 2 i)))
                          (tail "'" "char" i j))

  (define (string-lit i)  (define (loop i) (case (char-at src i)
                                                 ()    i
                                                 "\""  i
                                                 "\\"  (loop (+ 2 i))
                                                 _     (loop (+ 1 i))))
                          (define j (loop (+ 1 i)))
                          (tail "\"" "string" i j))

  (define (read-one i)
          (define c (char-at src i))
          (cond (pun? c)        (tag "pun" i (const ()))
                (digit? c)      (tag "int" i digit?)
                (equal? "'" c)  (char-lit i)
                (equal? "\"" c) (string-lit i)
                (id-char? c)    (tag "id" i id-char?)
                (op-char? c)    (tag "id" i op-char?)
                'else           (list (+ 1 i) "error" (implodes "not a token: " c))))

  (define (read-all line i out)
          (define c (char-at src i))
          (define loc (implodes fn ":" (integer->string line)))
          (cond (null? c)       (reverse (cons (list loc "end" "") out))
                (equal? c "\n") (read-all (+ 1 line) (+ 1 i) out)
                (space? c)      (read-all line (+ 1 i) out)
                (equal? c "#")  (read-all line (character-span not-eol? src i) out)
                'else           (let* res (read-one i)
                                      j   (car res)
                                      token (cons loc (cdr res))
                                      (read-all line j (cons token out)))))
  (define (process token)
          (case token
                (loc "id" id)     (cond (elem? id resv) (list loc id id)
                                        (upper? id)     (list loc "cid" id)
                                        'else           (list loc "id" id))
                (loc "int" s)     (list loc "int" (string->integer s))
                (loc "char" s)    (list loc "char" (unescape s))
                (loc "string" s)  (list loc "string" (unescape s))
                (loc "pun" s)     (list loc s s)
                (loc "error" s)   (begin (print "ml: error " loc ": " s "\n")
                                         (exit 1))
                _                 token))
  (map process (read-all 1 0 ())))



(define infixes ())
(define types ())
(define ctrs ())

(define (parse src)
  (define tloc  (first (car src)))
  (define ttype (second (car src)))
  (define ttxt (third (car src)))

  (define (error* loc . msg)
          (print "ml: error " loc ": " (implode msg) "\n")
          (exit 1))
  (define (error . msg) (apply error* (cons tloc msg)))

  (define (next)
          (case src
                ((loc type text) . ts)  (begin (set src ts)
                                               (set tloc loc)
                                               (set ttype type)
                                               (set ttxt text))
                ()                      ()))
  (define (peek type)
          (case src
                ((_ t* _) . _)  (equal? t* type)
                ()              ()))
  (define (want type) (if (peek type) (next) ()))
  (define (need type) (if (want type) 'true (error "need " type)))

  (define (top-level all)
          (cond (want "let")  (top-level-let all)
                'true         (error "need top-level decl")))

  (define (top-level-let all)
          (define rec?      (want "rec"))
          (defs      (list-of let-def "and"))
          (new-body  (fix-let-defs rec? defs ()))
          (append-to-let all new-body))

  (define (expr) (iexpr 0))

  (define (iexpr level)
          (if (> level 10)
              (cexpr)
              (iexpr (+ 1 level))))

  (define (cexpr)
          (cond (want "if")   (let loc tloc
                                   a (suffix (expr) "then")
                                   b (suffix (expr) "else")
                                   c (expr)
                                   (eif loc a b c))
                (want "case") (let loc tloc
                                   sub (expr)
                                   rules (list-of* case-rule)
                                   (ecase loc sub rules))
                (want "let")  (let loc tloc
                                   rec? (want "rec")
                                   defs (list-of let-def "and")
                                   _    (need "in")
                                   body (expr)
                                   (fix-let-defs rec? defs body))
                'application  (let f  (aexpr 'true)
                                   xs (list-of* argument)
                                   (fold-left eapp f xs))))

  (define (argument)
          (aexpr ()))

  (define (aexpr required?)
          (cond (want "int")    (eint tloc ttxt)
                (want "char")   (echr tloc (ord ttxt))
                (want "string") (estr tloc ttxt)
                (want "id")     (evar tloc ttxt)
                (want "cid")    (ectr tloc ttxt)
                (want "(")      (etup tloc (csv expr ")"))
                (want "[")      (fold-right econs
                                            (enil tloc)
                                            (csv expr "]"))
                (want "fn")     (fexpr)
                required?       (error "need expression")
                'else           ()))

  (define (case-rule)
          (if (want "|")
              (let lhs (suffix (expr) "->")
                   rhs (expr)
                   (list lhs rhs))
              ()))

  (define (let-def)
          (let  lhs (aexpr 'required)
                rhs (begin (want "=") (expr))
                (list lhs rhs)))

  (define (fexpr)
          (let* (arity? clause)   (length (car clause))
                (same-arity? n)   (lambda (i) (equal? (arity? i) n))
                (simple-arg? i)   (equal? (expr-type i) 'var)
                (simple-args? i)  (all? simple-arg? (car i))

                loc         tloc
                clauses     (list-of fn-clause "|")
                fst         (car clauses)
                arity       (arity? fst)
                wrong?      (not (all? (same-arity? arity) clauses))
                simple?     (all? simple-args? clauses)

                (cond wrong?    (error* loc "clauses must have same airty")
                      simple?   (fold-params (first fst) (second fst))
                      'complex  (fn->case clauses))))

  (define (fn-clause)
          (let args (suffix (list-of* argument) "->")
               body (expr)
               (list args body)))

  (define (suffix x token)
          (need token)
          x)

  (define (list-of get sep)
          (cons (get)
                (list-of* (lambda () (if (want sep) (get) ())))))

  (define (list-of* get)
          (case (get)
                ()    ()
                x     (cons x (list-of* get))))

  (define (csv get delim)
          (if (want delim)
              ()
              (let hd (get)
                   tl (if (want ",")
                          (csv get delim)
                          (begin (need delim)
                                 ()))
                   (cons hd tl))))

  (define (fix-let-defs rec? defs body)
          (if rec?
              (erec loc (map fix-rec-def defs))
              (fold-right breakup-let body defs)))

  (define (fix-rec-def def)
          (define lhs (first def))
          (define rhs (second def))
          (if (or (not-equal? 'var (expr-type lhs))
                  (not-equal? 'fn (expr-type rhs)))
              (error* (expr-loc lhs)
                      "rec only defines functions")
              ())
          (list (second lhs) rhs))

  (define (breakup-let r body)
          (elet (expr-loc (car r))
                (expr-etc (car r))
                (second r)      ; value of rule
                body))

  (define (fold-params params body)
          (define (make p e) (efn (expr-loc p) (expr-etc p) e))
          (fold-right make body params))

  (define (fn->case clauses)
          (let* (new-var e) (evar (expr-loc e) (uid))
                (param-loc c) (expr-loc (caar c))
                (->rule c)  (list (etup param-loc (car c))
                                  (second c))
                fst         (car clauses)
                loc         (param-loc fst)
                canon       (map new-var (car fst))
                sub         (etup loc canon)
                rules       (map ->rule clauses)
                _case       (ecase loc sub rules)
                (fold-params canon _case)))


  (expr))

(define (pp e)
        (define (pp-case-rule r) (implodes " | " (pp (first r)) " -> " (pp (second r))))
        (case e
              (_ 'nil)          "[]"
              (_ 'unit)         "()"
              (_ 'int x)        (integer->string x)
              (_ 'chr x)        (implodes "\'" (escape "'" (chr x)) "\'")
              (_ 'str x)        (implodes "\"" (escape "\"" x) "\"")
              (_ 'var x)        x
              (_ 'ctr x)        x
              (_ 'tup xs)       (implodes "(" (join "," (map pp xs)) ")")
              (_ 'cons x y)     (implodes "(" (pp x) ":" (pp y) ")")
              (_ 'app x y)      (implodes "(" (pp x) " " (pp y) ")")
              (_ 'if a b c)     (implodes "if " (pp a) " then " (pp b) " else " (pp c))
              (_ 'case e rs)    (implodes "case " (pp e) (implode (map pp-case-rule rs)))
              (_ 'let a b c)    (implodes "let " a "=" (pp b) " in " (pp c))
              (_ 'rec ds e)     (implodes "let rec " (map pp-rec-defs ds) " in " (pp e))
              (_ 'fn p e)       (implodes "fn " p "->" (pp e))
              _                 (begin (dbg "EXPR-NOT-PRINTED " e) (exit 1))
        ))

(define filename "test.ml")
; (define src (read-file filename))
(define src "let x=1 and y=2 in 3")
(define tokens (lex filename src))

(pr (pp (parse tokens)))
(begin (define (f x) (g (list 'f x)))
        (print 1)
        (define (g x) (h (list 'g x)))
        (print 2)
        (print 2)
        (print 2)
        (print 2)
        (print 2)
        (print 2)
        (print 2)
        (print 2)
        (print 2)
       (define (h x) (list (list 'h x) 10 20 30))
       (print (f 40)))

