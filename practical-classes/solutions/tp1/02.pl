pilot('Lamb').
pilot('Besenyei').
pilot('Chambliss').
pilot('MacLean').
pilot('Mangold').
pilot('Jones').
pilot('Bonhomme').


circuit('Istanbul').
circuit('Budapest').
circuit('Porto').


/* belongs_in_team(X,Y) --> X belongs in team Y */
belongs_in_team('Lamb', 'Breitling').
belongs_in_team('Besenyei' , 'Red Bull').
belongs_in_team('Chambliss' , 'Red Bull').
belongs_in_team('MacLean' , 'Mediterranean Racing Team').
belongs_in_team('Mangold' , 'Cobra').
belongs_in_team('Bonhomme' , 'Matador').
belongs_in_team('Jones' , 'Matador').


/* pilots(X,Y) --> X pilots Y */
pilots('Lamb' , 'MX2').
pilots('Besenyei' , 'Edge540').
pilots('Chambliss' , 'Edge540').
pilots('MacLean' , 'Edge540').
pilots('Mangold' , 'Edge540').
pilots('Jones' , 'Edge540').
pilots('Bonhomme' , 'Edge540').


/* number_of_gates(X,Y) --> X circuit has Y gates */
number_of_gates('Istanbul', 9).
number_of_gates('Budapest', 6).
number_of_gates('Porto', 5).


/* victory(X,Y) --> X won Y circuit */
victory('Jones' , 'Porto').
victory('Mangold' , 'Budapest').
victory('Mangold' , 'Istanbul').


/*

    a) victory(X, 'Porto').

    b) victory(X, 'Porto'), belongs_in_team(X, Y).

    c) victory(X, _Y), victory(X, _Z), _Y \= _Z.

    d) number_of_gates(X, _Y), _Y > 8.

    e) pilot(X), \+pilots(X, 'Edge540').

*/