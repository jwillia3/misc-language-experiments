(define (null? x) (equal? x ()))
(define (integer? x) (equal? (typeof x) 'integer))
(define (symbol? x) (equal? (typeof x) 'symbol))
(define (string? x) (equal? (typeof x) 'string))
(define (cons? x) (equal? (typeof x) 'cons))
(define (function? x) (equal? (typeof x) 'function))
(define (procedure? x) (equal? (typeof x) 'proc))

(define (not x) (if x () 'true))
(define (not-equal? x y) (if (equal? x y) () 'true))
(define (> x y) (< y x))
(define (>= x y) (<= y x))

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

(define (first x) (car x))
(define (second x) (car (cdr x)))
(define (third x) (car (cdr (cdr x))))
(define (fourth x) (car (cdr (cdr (cdr x)))))

(define (incr x) (+ x 1))
(define (decr x) (+ x 1))

(define (const k) (lambda (x) k))
(define (flip f) (lambda (x y) (f y x)))
(define (identity x) x)
(define (partial-apply function . captured-args)
        (define (append-args old new)
                (if (null? old)
                    new
                    (cons (car old) (append-args (cdr old) new))))
        (lambda new-args
                (apply function (append-args captured-args new-args))))

(define (pr . x)
        (define (cont last xs)
                (if (null? xs)
                    (begin  (print "\n")
                            x)
                    (begin (print (car xs))
                           (cont (car xs) (cdr xs)))))
        (cont () x))

(define (dbg . xs)
        (define (cont last xs)
                (if (null? xs)
                    (begin  (print "\n")
                            last)
                    (begin (print " " (car xs))
                           (cont (car xs) (cdr xs)))))
        (print "debug:")
        (cont () xs))

(define (fold-left combine leftmost lst)
        (case lst
              () leftmost
              (x . xs) (fold-left combine
                                  (combine leftmost x)
                                  xs)))
(define (fold-left* combine lst) (fold-left combine (car lst) (cdr lst)))

(define (reverse lst)
        (define (loop xs out)
                (if xs
                    (loop (cdr xs) (cons (car xs) out))
                    out))
        (loop lst ()))

(define (fold-right combine rightmost lst)
        (fold-left (flip combine) rightmost (reverse lst)))

(define (fold-right* combine lst)
        (fold-left* (flip combine) (reverse lst)))

(define (map xform lst)
        (fold-right (lambda (hd tl) (cons (xform hd) tl))
                    ()
                    lst))
(define (map xform lst)
        (define (loop xs out)
                (if xs
                    (loop (cdr xs) (cons (xform (car xs)) out))
                    (reverse out)))
        (loop lst ()))
(define (app do lst)
        (case lst
              ()        ()
              (x . lst) (begin (do x)
                               (app do lst))))

(define (append item lst) (fold-right cons lst item))

(define (flatten lst) (fold-right append () lst))

(define (flat-map xform lst) (flatten (map xform lst)))

(define (length lst)
        (fold-left (lambda (n _) (+ n 1)) 0 lst))

(define (nth lst n)
        (if (equal? n 0)
            (car lst)
            (nth (cdr lst) (- n 1))))

(define (set-nth lst n to)
        (if (equal? n 0)
            (setcar lst to)
            (set-nth (cdr lst) (- n 1) to)))

(define (elem? item lst)
        (if (null? lst)
            ()
            (or (equal? item (car lst))
                (elem? item (cdr lst)))))

(define (any? ok? lst)
        (if (null? lst)
            ()
            (or (ok? (car lst))
                (any? ok? (cdr lst)))))

(define (all? ok? lst)
        (if (null? lst)
            'true
            (and (ok? (car lst))
                 (all? ok? (cdr lst)))))

(define (zip xs ys)
        (define (loop xs ys out)
                (if (or (null? xs) (null? ys))
                    (reverse out)
                    (loop (cdr xs)
                          (cdr ys)
                          (cons (cons (car xs) (car ys)) out))))
        (loop xs ys ()))

(define (unpair f) (lambda (pair) (f (car pair) (cdr pair))))

(define (implodes . strings) (implode strings))

(define (lower? c) (and (<= c "z") (<= "a" c)))
(define (upper? c) (and (<= c "Z") (<= "A" c)))
(define (alpha? c) (or (lower? c) (upper? c)))
(define (digit? c) (and (<= c "9") (<= "0" c)))
(define (alnum? c) (or (alpha? c) (digit? c)))
(define (space? c) (or (equal? c " ") (equal? c "\t") (equal? c "\n")))
(define (toupper string)
        (define (cont i out)
                (if (< i (string-length string))
                    (let* c (char-at string i)
                          c (if (lower? c) (chr (- (ord c) 32)) c)
                          (cont (+ i 1) (cons c out)))
                    (implode (reverse out))))
        (cont 0 ()))
(define (tolower string)
        (define (cont i out)
                (if (< i (string-length string))
                    (let* c (char-at string i)
                          c (if (upper? c) (chr (+ (ord c) 32)) c)
                          (cont (+ i 1) (cons c out)))
                    (implode (reverse out))))
        (cont 0 ()))

(define (explode string)
        (define (cont i out)
                (if (< i (string-length string))
                    (cont (+ i 1) (cons (char-at string i) out))
                    (reverse out)))
        (cont 0 ()))

(define (unescape string)
        (define (cont i out)
                (case (char-at string i)
                      ()    (implode (reverse out))
                      "\\"  (begin  (define c  (case (char-at string (+ i 1))
                                                     "t"  "\t"
                                                     "n"  "\n"
                                                     c    c))
                                    (cont (+ i 2) (cons c out)))
                      c     (cont (+ i 1) (cons c out))))
        (cont 0 ()))

(define (escape quote string)
        (define (cont i out)
                (if (< i (string-length string))
                    (cont (+ i 1)
                          (cons (case (char-at string i)
                                      "\\" "\\\\"
                                      "\n" "\\\n"
                                      "\t" "\\\t"
                                      "\"" "\\\""
                                      "\'" "\\\'"
                                    c    c)
                                out))
                    (implode (reverse out))))
        (cont 0 ()))

(define (integer->string number)
        (define (cont n)
                (if (<= n 0)
                     ()
                     (cons (chr (+ (ord "0")
                                   (remainder n 10)))
                           (cont (/ n 10)))))
        (if (equal? number 0)
            "0"
            (implode (reverse (cont number)))))

(define (string->integer string)
        (define (cont i out)
                (define c (char-at string i))
                (if (digit? c)
                    (cont (+ i 1)
                          (+ (- (ord c) (ord "0"))
                             (* out 10)))
                    out))
        (cont 0 0))

(define (substring-equal larger base substring)
        (define (cont i)
                (if (< i (string-length substring))
                    (if (equal? (char-at larger (+ base i)) (char-at substring i))
                        (cont (+ i 1))
                        ())
                    'true))
        (cont 0))

(define (starts-with larger prefix)
        (substring-equal larger 0 prefix))

(define (ends-with larger suffix)
        (define base (- (string-length larger) (string-length suffix)))
        (if (< base 0)
            ()
            (substring-equal larger base suffix)))

(define (find-string-after larger base substring)
        (define (cont i)
                (if (< (+ base i) (string-length larger))
                    (if (substring-equal larger (+ base i) substring)
                        (+ base i)
                        (cont (+ i 1)))
                    ()))
        (cont 0))

(define (find-string larger substring)
        (find-string-after larger 0 substring))

(define (character-span ok? string i)
        (define (cont i)
                (if (and (< i (string-length string)) (ok? (char-at string i)))
                    (cont (+ i 1))
                    i))
        (cont i))

(define (join delimiter strings)
        (define (cont lst out)
                (if (null? lst)
                    (implode (reverse out))
                    (cont (cdr lst) (cons (car lst) (cons delimiter out)))))
        (if (null? strings)
            ""
            (cont (cdr strings) (list (car strings)))))

(define (split delimiter string)
        (define d-len (string-length delimiter))
        (define (cont i out)
                (if (< i (string-length string))
                    (case (find-string-after string i delimiter)
                          ()  (cont (string-length string)
                                    (cons (substring string i -1) out))
                          j   (cont (+ j d-len)
                                    (cons (substring string i j) out)))
                    (reverse out)))
        (cont 0 ()))
(define (repeat-string string n)
        (define (cont i out)
                (if (< i n)
                    (cont (- i 1) (cons string out))
                    (implode (reverse out))))
        (cont 0 ()))

