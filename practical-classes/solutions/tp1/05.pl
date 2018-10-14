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

%X gosta de Y
gosta('Joao','natacao').
gosta('Joao','xadrez').
gosta('Maria','tenis').
gosta('Maria','tigre').
gosta('Ana', 'gato').


/*

    a) moraEm(X, 'apartamento'), gosta(X, _Y), animal(_Y).
    
    b) moraEm('Joao', 'casa'), moraEm('Maria', 'casa'), gosta('Joao', _X), desporto(_X), gosta('Maria', _Y), desporto(_Y).
    
    c) gosta(X, _Y), gosta(X, _Z), desporto(_Y), jogo(_Z).
    
    d) mulher(X), gosta(X, 'tenis'), gosta(X, 'tigre').

*/
