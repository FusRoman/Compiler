(*
  La version Alloc du programme n'a pas été faite.
  Nous avons fait quelques tentatives dans ce sens, mais suite à une perte de données et parce que nous 
  devions nous occuper de IMP, cette optimisation n'a jamais été finie.

  Cependant, nous avons déjà une idée assez claire de comment nous allons l'implémenter.
  Les constantes suivantes sont déclarées :
    - sp : le registre stack_pointer, conservé depuis la version Acc
    - max_reg, le nombre de registres qu'on peut utiliser pour simuler la pile.

  Pendant la seconde passe, à chaque nouvelle instruction compilée, on met à jour les variables suivantes :
    - stack_depth : la profondeur de la pile actuelle
    - unused_regs : le nombre de registres inutilisés

  Lorsqu'on rencontre un tag ('tag:'), on suit les règles suivantes :
      - stack_depth est réinitialisé à 0, puisqu'on suit la supposition de l'énoncé
      - en conséquence, unused_regs passe à max_reg.

  On définit le registre courant (pour une instruction donnée) comme stack_depth % max_reg.
  Ce registre sera toujours celui qu'on utilisera quand on veut empiler une valeur sur la pile.
  Un registre est dit occupé si on considère qu'il contient une valeur de la pile.
  Si au contraire on n'accorde plus de sens à sa valeur, on dit qu'il est libre ou disponible.

  Chaque instruction a un effet particulier sur la type qui dépend de sa 'signature'. Par exemple,
  on considère MINUS comme une fonction unaire car elle prend un élément de la pile et en empile un.
  PRINT est une procédure unaire, car elle ne retourne rien. On peut donc classer les instructions selon
  leur signature. C'est ce qui a été fait dans la version Acc en prévision, bien que cette version n'en tire
  aucun avantage.

  Pour le pop,on sait que la valeur précédente est contenue dans un registre et pas la pile physique avec
  unused_regs < max_reg. Autrement dit, au moins un registre est occupé ; par construction, il s'agit du dernier.
  Si unused_regs >= max_reg ou, de manière équivalente, unused_regs = max_reg, alors il va falloir dépiler 
  un élément de la pile physique : on décrémente stack_depth et incrémente unused_regs.
  On place le résultat dans (stack_depth - 1) % max_reg (en supposant que la soustraction est circulaire ;
  le calcul concret est donc un peu plus long).
  Si on a besoin d'une deuxième valeur, on procède de manière similaire.

  Si on veut au contraire empiler une valeur, alors on vérifie que le registre courant est libre avec
  unused_regs > 0. Par construction, si un registre est libre, alors le registre courant l'est forcément.
  On incrémente stack_depth et décrémente unused_regs. Si ce registre n'est pas disponible, on empile
  sa valeur.

  Evidemment, puisque nous n'avons jamais testé cet algorithme, il est probable qu'il ne fonctionne pas 
  et qu'il faille l'adapter pour le rendre fonctionnel. Nous projetons toujours de l'implémenter,
  même si c'est trop tard pour le STK.

  A cause de nos suppositions que nous avons interprété de l'énoncé, tous les programmes STK pourtant valides
  ne pourront pas être correctement compilés par Alloc, en particulier le suivant :
  1ère boucle, 10 itérations :
    a # On empile a en excédent ; le reste de la boucle laisse la pile dans le même état qu'au début
  2ème boucle, 10 itérations :
    PRINT



  Nous avons également des ébauches d'algorithme pour pousser l'optimisation plus loin.
  La motivation principale est d'optimiser les empilements récurrents d'une même valeur,
  inhérents au fonctionnement de STK.
  L'algorithme se reposerait sur les "séquences autonomes", c'est-à-dire une suite d'instructions
  qui, ayant commencée à une profondeur de pile p, ne dépile jamais une valeur à une profondeur q < p.
  On découpe le programme en blocs commençant à la déclaration d'une étiquette et finissant juste avant la prochaine
  (de manière similaire à Alloc).

  On utiliserait un arbre pour stocker les séquences autonomes, et les relier à une variable et à un code compressible.
  Le code compressible est constitué d'instructions incompressibles (dont NOP et PRINT font toujours partie, par exemple)
  et de séquences dont le résultat a été stocké dans une variable (et donc potentiellement compressibles).
  Une pile virtuelle où chaque élément est représenté par le code l'ayant généré serait également nécessaire
  pour la construction de cet arbre, pour pouvoir correctement traiter les fonctions et procédures binaires.

  La première étape consiste à identifier les codes compressibles et à dresser une liste des variables et leurs occurences 
  dans un bloc.
  La deuxième tente ensuite d'attribuer à chaque variable un registre de façon à minimiser le nombre de fois
  où on va devoir recalculer un même nombre, faute de place.

  L'empilement d'étiquette, les WRITE et les JUMP/JUMPWHEN sont des cas spéciaux qui compliquerait cet algorithme.
  Tout est remis à 0 en rencontrant une déclaration d'étiquette.
*)