homem('Joao').

mulher('Maria').
mulher('Ana').

animal('cao').
animal('gato').
animal('tigre').

jogo('xadrez').
jogo('damas').

desporto('tenis').
desporto('natacao').

moraEm('Joao','casa').
moraEm('Maria','casa').
moraEm('Ana','apartamento').

gostaDe('Joao','natacao').
gostaDe('Joao','xadrez').
gostaDe('Maria','tenis').
gostaDe('Maria','tigre').
gostaDe('Ana', 'gato').


/*

    a) moraEm(X, 'apartamento'), gostaDe(X, _Y), animal(_Y).
    
    b) moraEm('Joao', 'casa'), moraEm('Maria', 'casa'), gostaDe('Joao', _X), desporto(_X), gostaDe('Maria', _Y), desporto(_Y).
    
    c) gostaDe(X, _Y), gostaDe(X, _Z), desporto(_Y), jogo(_Z).
    
    d) mulher(X), gostaDe(X, 'tenis'), gostaDe(X, 'tigre').

*/
