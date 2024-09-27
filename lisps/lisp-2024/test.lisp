(LETREC
    ; APPLY FUNCTION TO EACH ELEMENT OF THE LIST.
    ((APPLY-LIST F XS)
        (COND
            ((EQUAL XS NIL)
                NIL)
            (TRUE
                (BEGIN
                    (F (CAR XS))
                    (APPLY-LIST F (CDR XS))))))

    ; MAP FUNCTION TO EACH ELEMENT OF THE LIST.
    ((MAP F XS)
        (LETREC
            ((LOOP XS OUT)
                (COND
                    ((EQUAL XS NIL)
                        OUT)
                    (TRUE
                        (LOOP (CDR XS) (CONS (F (CAR XS)) OUT)))))
            (LOOP XS NIL)))

    ; PRINT WITH LINEBREAK.
    ((PRN . XS)
        (APPLY-LIST PR XS)
        (PR "\n"))

    ; RETURN THE LIST OF ARGUMENTS.
    ((ENLIST . XS) XS)

    ; COUNT THE ELEMENTS OF A LIST.
    ((COUNT XS)
        (COND
            ((EQUAL XS NIL)
                0)
            (TRUE
                (ADD 1 (COUNT (CDR XS))))))

    (BEGIN
        (PRN (MAP (LAMBDA(X) (ADD X 100)) (ENLIST 1 2 3)))
        (PR "\n")
        TRUE))
