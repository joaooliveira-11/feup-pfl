% gender
female(grace).
female(dede).
female(gloria).
female(barb).
female(claire).
female(pameron).
female(haley).
female(alex).
female(lily).
female(poppy).
male(frank).
male(jay).
male(javier).
male(merle).
male(phil).
male(mitchell).
male(joe).
male(manny).
male(cameron).
male(bo).
male(dylan).
male(luke).
male(rexford).
male(calhoun).
male(george).

% family
parent(grace, phil).
parent(frank, phil).
parent(dede, claire).
parent(jay, claire).
parent(dede, mitchell).
parent(jay, mitchell).
parent(jay, joe).
parent(gloria, joe).
parent(javier, manny).
parent(gloria, manny).
parent(barb, cameron).
parent(merle, cameron).
parent(barb, pameron).
parent(merle, pameron).
parent(phil, haley).
parent(claire, haley).
parent(phil, alex).
parent(claire, alex).
parent(phil, luke).
parent(claire, luke).
parent(mitchell, lily).
parent(cameron, lily).
parent(mitchell, rexford).
parent(cameron, rexford).
parent(pameron, calhoun).
parent(bo, calhoun).
parent(dylan, george).
parent(haley, george).
parent(dylan, poppy).
parent(haley, poppy).

% relations
father(X, Y) :- parent(X, Y), male(X).
mother(X, Y) :- parent(X, Y), female(X).
grandparent(X, Y) :- parent(X, Z), parent(Z, Y).
grandchildren(X,Y) :- parent(Z, X), parent(Y, Z).
siblings(X,Y) :- father(A, X), mother(B, X), father(A, Y), mother(B, Y), X \= Y.
halfsiblings(X,Y) :- \+ siblings(X,Y), parent(P, X), parent(P,Y), X \= Y.
cousin(X,Y) :- parent(P1, X), parent(P2, Y), siblings(P1, P2), X \= Y, P1 \= P2. 
uncle(X,Y) :- parent(P,Y), siblings(P, X), male(X).

% marriages and divorces

marriage(jay, gloria, 2008).
marriage(jay, dede, 1968).
divorce(jay, dede, 2003).

married(P1,P2,Y) :- marriage(P1,P2,Y).
divorced(P1,P2,Y) :- divorce(P1,P2,Y).

% teachers and students

teaches(algorithms, adalberto).
teaches(databases, bernardete).
teaches(compilers, capitolino).
teaches(statistics, diogenes).
teaches(networks, ermelinda).

attends(algorithms , alberto).
attends(algorithms , bruna).
attends(algorithms , cristina).
attends(algorithms , diogo).
attends(algorithms , eduarda).
attends(databases, antonio).
attends(databases, bruno).
attends(databases, cristina).
attends(databases, duarte).
attends(databases, eduardo).
attends(compilers, alberto).
attends(compilers, bernardo).
attends(compilers, clara).
attends(compilers, diana).
attends(compilers, eurico).
attends(statistics, antonio).
attends(statistics, bruna).
attends(statistics, claudio).
attends(statistics, duarte).
attends(statistics, eva).
attends(networks, alvaro).
attends(networks, beatriz).
attends(networks, claudio).
attends(networks, diana).
attends(networks, eduardo).

% relations

professor(X) :- teaches(Z, X).
student(X) :- attends(Z, X).
professor(X,Y) :- teaches(Z,X), attends(Z, Y).
student(X,Y) :- attends(Z, X), teaches(Z, Y).


% Red Bull Air Race

pilot(lamb).
pilot(besenyei).
pilot(chambliss).
pilot(macLean).
pilot(mangold).
pilot(jones).
pilot(bonhomme).

team(lamb, breitling).
team(besenyei, red_bull).
team(chambliss, red_bull).
team(maclean, mediterranean_racing_team).
team(mangold, cobra).
team(jones, matador).
team(bonhomme, matador).

pilots(lamb, mx2).
pilots(besenyei, edge540).
pilots(chambliss, edge540).
pilots(maclean, edge540).
pilots(mangold, edge540).
pilots(jones, edge540).
pilots(bonhomme, edge540).

circuit(istanbul).
circuit(budapest).
circuit(porto).

circuit_winner(porto, jones).
circuit_winner(budapest, mangold).
circuit_winner(istanbul, mangold).

nr_gates(istanbul, 9).
nr_gates(budapest, 6).
nr_gates(porto, 5).







