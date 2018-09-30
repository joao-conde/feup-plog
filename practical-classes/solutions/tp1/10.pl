comprou(joao, honda).
comprou(joao, uno).

ano(honda, 1997).
ano(uno, 1998).

valor(honda, 20000).
valor(uno, 7000).

/*
    Pessoa só pode vender Carro se for comprado por ela nos
    últimos 10 anos e se o valor for menor do que 10.000 euros
*/
podeVender(Pessoa, Carro, AnoAtual):-
    comprou(Pessoa, Carro),
    valor(Carro, V),
    V < 10000,
    ano(Carro, AnoCompra),
    AnoAtual - AnoCompra =< 10.

