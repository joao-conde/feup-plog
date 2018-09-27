:-use_module(library(clpfd)).



carros:- length(Cores,4), domain(Cores,1,4),
         length(Tam,4), domain(Tam,1,4),
         all_distinct(Tam),all_distinct(Cores),
         element(IdxVerde,Cores,3),
         element(IdxVerde,Tam,1),
         element(IdxAzul,Cores,1),
         IdxVerde #> IdxAzul,
         element(IdxAmarelo,Cores,2),
         element(IdxPreto,Cores,4),
         IdxAmarelo #> IdxPreto,
         AntesAzul #= IdxAzul-1,
         DepoisAzul #= IdxAzul+1,
         element(AntesAzul,Tam,Peq),
         element(DepoisAzul,Tam,Gr),
         Peq #< Gr,
         labeling([],Cores),
         write(Cores).

cars2:- length(Cars,12),
        domain(Cars, 1, 4),
        global_cardinality(Cars, [1-3,2-3,3-4,4-3]),
        element(1, Cars, C1),
        element(12, Cars, C1).