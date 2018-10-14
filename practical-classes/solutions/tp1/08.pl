%X é um cargo de Y
cargo(tecnico, rogerio).
cargo(tecnico, ivone).
cargo(engenheiro, daniel).
cargo(engenheiro, isabel).
cargo(engenheiro, oscar).
cargo(engenheiro, tomas).
cargo(engenheiro, ana).
cargo(supervisor, luis).
cargo(supervisor_chefe, sonia).
cargo(secretaria_exec, laura).
cargo(diretor, santiago).

%Cargo X chefiado por cargo Y
chefiado(tecnico, engenheiro).
chefiado(engenheiro, supervisor).
chefiado(analista, supervisor).
chefiado(supervisor, supervisor_chefe).
chefiado(supervisor_chefe, director).
chefiado(secretaria_exec, director).


/*

    a) Quem chefia o chefe dos tecnicos?

        X = engenheiro, 
        Y = supervisor ?

    b) A ivone chefia tecnicos?

        no

    c) Existe algum supervisor?

        X = luis ? 

    d) Quem possui um cargo chefiado pelo supervisor chefe ou pelo supervisor?

        J = engenheiro,
        P = daniel ? 

    e) Que cargo é chefiado pelo diretor e nao pertence à Carolina?

    PS: USAR \+ em vez do predicado not/1

        P = supervisor_chefe ?


*/