% child(Child, Mother, Father, ChildsSex)
child(princess_charlotte_of_cambridge, catherine_duchess_of_cambridge, prince_william_duke_of_cambridge, female).
child(mia_tindall, zara_tindall, mike_tindall, female).
child(prince_louis_of_cambridge, catherine_duchess_of_cambridge, prince_william_duke_of_cambridge, male).
child(jack_brooksbank, nicola_newton, george_brooksbank, male).
child(archie_mountbatten-windsor, meghan_duchess_of_sussex, prince_harry_duke_of_sussex, male).
child(lena_tindall, zara_tindall, mike_tindall, female).
child(lola_rosalind_parker_bowles, sara_buys, tom_parker_bowles, female).
child(eliza_lopes, laura_lopes, harry_marcus_george_lopes, female).
child(gus_lopes, laura_lopes, harry_marcus_george_lopes, male).
child(louis_lopes, laura_lopes, harry_marcus_george_lopes, male).
child(freddy_parker_bowles, sara_buys, tom_parker_bowles, male).
child(elizabeth_ii, queen_elizabeth_the_queen_mother, george_vi, female).
child(catherine_duchess_of_cambridge, carole_middleton, michael_middleton, female).
child(prince_william_duke_of_cambridge, diana_princess_of_wales, charles_prince_of_wales, male).
child(charles_prince_of_wales, elizabeth_ii, prince_philip_duke_of_edinburgh, male).
child(sarah_duchess_of_york, susan_barrantes, ronald_ferguson, female).
child(prince_philip_duke_of_edinburgh, princess_alice_of_battenberg, prince_andrew_of_greece_and_denmark, male).
child(zara_tindall, anne_princess_royal, mark_phillips, female).
child(anne_princess_royal, elizabeth_ii, prince_philip_duke_of_edinburgh, female).
child(camilla_duchess_of_cornwall, rosalind_cubitt, bruce_shand, female).
child(prince_harry_duke_of_sussex, diana_princess_of_wales, charles_prince_of_wales, male).
child(prince_andrew_duke_of_york, elizabeth_ii, prince_philip_duke_of_edinburgh, male).
child(prince_edward_earl_of_wessex, elizabeth_ii, prince_philip_duke_of_edinburgh, male).
child(sophie_countess_of_wessex, mary_o_sullivan, christopher_rhys-jones, female).
child(princess_beatrice_of_york, sarah_duchess_of_york, prince_andrew_duke_of_york, female).
child(princess_eugenie_of_york, sarah_duchess_of_york, prince_andrew_duke_of_york, female).
child(peter_phillips, anne_princess_royal, mark_phillips, male).
child(autumn_phillips, kathleen_mccarthy, brian_kelly, female).
child(james_viscount_severn, sophie_countess_of_wessex, prince_edward_earl_of_wessex, male).
child(lady_louise_windsor, sophie_countess_of_wessex, prince_edward_earl_of_wessex, female).
child(mike_tindall, linda_m_shepherd, philip_j_tindall, male).
child(savannah_phillips, autumn_phillips, peter_phillips, female).
child(meghan_duchess_of_sussex, doria_ragland, thomas_markle, female).
child(tom_parker_bowles, camilla_duchess_of_cornwall, andrew_parker_bowles, male).
child(laura_lopes, camilla_duchess_of_cornwall, andrew_parker_bowles, female).
child(isla_phillips, autumn_phillips, peter_phillips, female).

% Zad 1
father(Father, Child) :- child(Child, _, Father, _).
mother(Mother, Child) :- child(Child, Mother, _, _).
parent(Parent, Child) :- child(Child, Parent, _, _); child(Child, _, Parent, _).
grandma(Child, Grandma) :- child(Child, Mother, _, _), child(Mother, Grandma, _, _);
                           child(Child, _, Father, _), child(Father, Grandma, _, _).
grandpa(Child, Grandpa) :- child(Child, Mother, _, _), child(Mother, _, Grandpa, _);
                           child(Child, _, Father, _), child(Father, _, Grandpa, _).
grandchild(Grandchild, Grandparent) :- grandma(Grandchild, Grandparent);
                                       grandpa(Grandchild, Grandparent).
ancestor(Ancestor, Descendant) :- parent(Ancestor, Descendant);
                                  parent(Ancestor, X), ancestor(X, Descendant).

% Zad 2
son(Child, Mother, Father) :- child(Child, Mother, Father, male).
daughter(Child, Mother, Father) :- child(Child, Mother, Father, female).
chld(Child, Mother, Father) :- child(Child, Mother, Father, _).
granddaughter(Child, Grandparent) :- child(Child, Mother, _, female), child(Mother, Grandparent, _, _);
                                     child(Child, Mother, _, female), child(Mother, _, Grandparent, _);
                                     child(Child, _, Father, female), child(Father, Grandparent, _, _);
                                     child(Child, _, Father, female), child(Father, _, Grandparent, _).
grandson(Child, Grandparent) :- child(Child, Mother, _, male), child(Mother, Grandparent, _, _);
                                child(Child, Mother, _, male), child(Mother, _, Grandparent, _);
                                child(Child, _, Father, male), child(Father, Grandparent, _, _);
                                child(Child, _, Father, male), child(Father, _, Grandparent, _).

% Zad 3
nat(z).
nat(s(X)) :- nat(X).

plus(z, Y, Y).
plus(s(X), Y, s(Z)) :- plus(X, Y, Z).

minus(X, z, X).
minus(X, s(Y), Z) :- minus(X, Y, s(Z)).

fib(z, z).
fib(s(z), s(z)).
fib(s(s(X)), Z) :- fib(X, A), fib(s(X), B), plus(A, B, Z).

% Zad 4
lista([]).
lista([_|T]) :- lista(T).

pierwszy(E, [E|_]).

ostatni(E, [E]).
ostatni(E, [_|T]) :- ostatni(E,T).

element(E, [E|_]).
element(E, [_|T]) :- element(E, T).

scal([], L2, L2).
scal([H|T], L2, [H|L3]) :- scal(T, L2, L3).

intersect([H|T], Z2) :- element(H, Z2); intersect(T, Z2).

podziel([], [], []).
podziel([H], [H], []).
podziel([H1|[H2|T]], [H1|NP], [H2|P]) :- podziel(T, NP, P).

prefiks([], _).
prefiks([H|T1], [H|T2]) :- prefiks(T1, T2).

podlista([], _).
podlista([H|T1], [H|T2]) :- prefiks(T1, T2).
podlista([H|T1], [_|T2]) :- podlista([H|T1], T2).

podciag([], _).
podciag([H|T1], [H|T2]) :- podciag(T1, T2).
podciag([H|T1], [_|T2]) :- podciag([H|T1], T2).

wypisz([]) :- write('Lista pusta').
wypisz([H]) :- write(H), write('.'), !.
wypisz([H|T]) :- write(H), write(', '), wypisz(T).

insert([], E, [E]).
insert([H|T1], E, [H|T2]) :- H < E, insert(T1, E, T2).
insert(L, E, [E|L]).

insertionSort([], []).
insertionSort([H|T], Z) :- insertionSort(T, Y), insert(Y, H, Z).

srodek(E, [E]).
srodek(E, [_|T]) :- append(S, [_], T), srodek(E, S).
