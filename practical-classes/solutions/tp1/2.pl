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


/* belongsInTeam(X,Y) --> X belongs in team Y */
belongsInTeam('Lamb', 'Breitling').
belongsInTeam('Besenyei' , 'Red Bull').
belongsInTeam('Chambliss' , 'Red Bull').
belongsInTeam('MacLean' , 'Mediterranean Racing Team').
belongsInTeam('Mangold' , 'Cobra').
belongsInTeam('Bonhomme' , 'Matador').
belongsInTeam('Jones' , 'Matador').


/* planePilot(X,Y) --> X pilots Y */
planePilot('Lamb' , 'MX2').
planePilot('Besenyei' , 'Edge540').
planePilot('Chambliss' , 'Edge540').
planePilot('MacLean' , 'Edge540').
planePilot('Mangold' , 'Edge540').
planePilot('Jones' , 'Edge540').
planePilot('Bonhomme' , 'Edge540').


/* numberOfGates(X,Y) --> X circuit has Y gates */
numberOfGates('Istanbul', 9).
numberOfGates('Budapest', 6).
numberOfGates('Porto', 5).


/* victory(X,Y) --> X won Y circuit */
victory('Jones' , 'Porto').
victory('Mangold' , 'Budapest').
victory('Mangold' , 'Istanbul').


/*

    a) victory(X, 'Porto').

    b) victory(X, 'Porto'), belongsInTeam(X, Y).

    c) victory(X, _Y), victory(X, _Z), _Y \= _Z.

    d) numberOfGates(X, _Y), _Y > 8.

    e) pilot(X), \+planePilot(X, 'Edge540').

*/