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


% (1) c) relations

father(Father, Child) :-
    male(Father), parent(Father, Child).

mother(Mother, Child) :-
    female(Mother), parent(Mother, Child).

grandparent(Grandparent, Grandchild) :-
    parent(Grandparent, Parent), parent(Parent, Grandchild).

grandmother(Grandmoder, Grandchild) :-
    mother(Grandmoder, Parent), parent(Parent, Grandchild).

siblings(Person, Sibling) :-
    father(Father, Person), mother(Mother, Person), father(Father, Sibling), mother(Mother, Sibling), Sibling \= Person.

halfsiblings(Person, Halfsibling) :-
    parent(Parent, Person), parent(Parent, Halfsibling), \+ siblings(Person, Halfsibling), Person \= Halfsibling.


% (1) e) relations

marriage(jay, dede, 1968).
marriage(jay, gloria, 2008).
divorce(jay, dede, 2003).

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

% (2) c)

student(Student, Professor) :-
    teaches(Course, Professor), attends(Course, Student).

student(Student, Professor, Professor1) :-
    teaches(Course, Professor), teaches(Course1, Professor1), attends(Course, Student), attends(Course1, Student), Professor \= Professor1.

colleagues(Person1, Person2) :-
    Person1 @< Person2,
    (
        teaches(_, Person1), teaches(_, Person2), Person1 \= Person2
    ;
        attends(Course, Person1), attends(Course, Person2), Person1 \= Person2
    ).

attend_many_courses(Student) :-
    attends(Course1, Student), attends(Course2, Student), Course1 \= Course2.

% Red Bull Air Race

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

% (4)

translate(1, 'Integer Overflow').
translate(2, 'Division by zero').
translate(3, 'ID Unkwown').


% Jobs and Bosses

job(technician, eleuterio).
job(technician, juvenaldo).
job(analyst, leonilde).
job(analyst, marciliano).
job(engineer, osvaldo).
job(engineer, porfirio).
job(engineer, reginaldo).
job(supervisor, sisnando).
job(chief_supervisor, gertrudes).
job(secretary, felismina).
job(director, asdrubal).
supervised_by(technician, engineer).
supervised_by(engineer, supervisor).
supervised_by(analyst, supervisor).
supervised_by(supervisor, chief_supervisor).
supervised_by(chief_supervisor, director).
supervised_by(secretary, director).

% 5 c)

direct_supervisor(X,Y) :-
    job(Xjob, X), job(Yjob, Y),
    supervised_by(Yjob, Xjob).

both_supervised(X,Y) :-
    job(Xjob, X), job(Yjob, Y),
    supervised_by(Xjob, Supervisor),
    supervised_by(Yjob, Supervisor).

multiple_supervisor(X):-
    job(_Xjob, X),
    supervised_by(_Job, _Xjob),
    supervised_by(_Job1, _Xjob),
    _Job \= _Job1.

double_supervisor(X,Y) :-
    job(Xjob, X), job(Yjob, Y),
    supervised_by(Yjob, Ysupervisor),
    supervised_by(Ysupervisor, Xjob).