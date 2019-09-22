    # Borne et  compteur
    CONST  $r0 5
    CONST  $r1 0
    # Saut à l’étiquette  ‘‘test ’’
    ADDRESS  $r2  test
    JUMP  $r2
loop:
    # Affichage  de la  valeur  de ‘‘a’’
    DIRECTREAD  $r3 a
    PRINT  $r3
    # Incrément et  sauvegarde  de ‘‘a’’
    INCR  $r3 2
    DIRECTWRITE a $r3
    # Incrément du  compteur
    INCR  $r1 1
test:
    # Test de non-dépassement  de la borne
    LT $r3 $r1 $r0
    # Saut à l’étiquette  ‘‘loop ’’ si non -dépassement
    ADDRESS  $r2  loop
    JUMP  $r2  WHEN  $r3
    # Affichage  retour à la  ligne
    CONST  $r0 10
    PRINT  $r0
    # Fin
    EXIT
# Une  donnée allouée statiquement
a:
    97
