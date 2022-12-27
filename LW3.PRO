DOMAINS
    person = symbol
PREDICATES
	woman(person)
	man(person)
	parent(person, person)

	father(person, person)
	mother(person, person)

	pair(person, person)
	wife(person, person)
	husband(person, person)

	daughter(person, person)
	son(person, person)
	child(person, person)

	siblings(person, person)
	sister(person, person)
	brother(person, person)

	zyat(person, person)
	shurin(person, person)
	uncle(person, person)

	grandpa(person, person)
CLAUSES
	man(me).
	man(f).
	man(w_s).
	man(w_d_s).
	woman(w).
	woman(w_d).
	parent(w, w_d).
	parent(me, w_d).
	parent(f, me).
	parent(w_d, me).
	parent(w, w_s).
	parent(me, w_s).
	parent(w_d, w_d_s).
	parent(f, w_d_s).

	father(X, Y) :- man(X), parent(X, Y).
	mother(X, Y) :- woman(X), parent(X, Y).

	pair(X, Y) :- parent(X, Z), parent(Y, Z), X <> Y.
	wife(X, Y) :- woman(X), pair(X, Y).
	husband(X, Y) :- man(X), pair(X, Y).

	daughter(X, Y) :- woman(X), parent(Y, X).
	son(X, Y) :- man(X), parent(Y, X).
	child(X, Y) :- daughter(X, Y); son(X, Y).

	siblings(X, Y) :- parent(Z, X), parent(Z, Y), X <> Y.
	sister(X, Y) :- woman(X), siblings(X, Y).
	brother(X, Y) :- man(X), siblings(X, Y).

	zyat(X, Y) :- husband(X, Z), daughter(Z, Y);
		husband(X, Z), sister(Z, Y).
	shurin(X, Y) :- brother(X, Z), wife(Z, Y).
	uncle(X, Y) :- brother(X, Z), parent(Z, Y).

	grandpa(X, Y) :- father(X, Z), parent(Z, Y).
