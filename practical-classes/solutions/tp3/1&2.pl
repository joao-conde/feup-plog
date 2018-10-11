/* a) */ [a|[b,c,d]] = [a,b,c,d].
/* b) */ [a|b,c,d] = [a,b,c,d].
/* c) */ [H|T] = [apple, broccoli, refrigerator].
/* d) */ [H|T] = [a, b, c, d, e].
/* e) */ [H|T] = [apples, bananas].
/* f) */ [H|T] = [a, [b,c,d]].
/* g) */ [H|T] = [apples].
/* h) */ [H|T] = [].
/* i) */ [One, Two | T] = [apple, sprouts, fridge, milk].
/* j) */ [X,Y|T] = [a|Z].
/* k) */ [H|T] = [apple, Z].
/* l) */ [a|[b|[c|[d|[]]]]] = [a,b,c,d].

/*
	a) yes 
	b) no 
	c) H = apple 
	    T = [broccoli, refrigerator]  
	d) H = a 
	    T = [b, c, d, e]  
	e) H = apples 
	    T = [bananas]  
	f) H = a 
	    T = [[b, c, d]]  
	g) H = apples 
	    T = []  
	h) no 
	i) One = apple 
	    Two = sprouts
	    T = [fridge, milk]
	j) X = a 
	    Y = _01 
	    T = _03 
	    Z = [_01 | _03]  
	k) H = apple 
	    T = [_01] 
	    Z = _01  
	l) yes

*/