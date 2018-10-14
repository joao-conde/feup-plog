my_append([],L,L). 
my_append([X|L1],L2,[X|L3]):- my_append(L1,L2,L3).