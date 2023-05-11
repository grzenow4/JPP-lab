% ---------- KOLEJKA ---------- %

dappend(X-Y, Y-Z, X-Z).

init(X-X).

get(E, [E|X]-Y, X-Y).

put(E, X-[E|Y], X-Y).

empty(X-X) :- var(X).

putAll([], Q, Q).
putAll([H|T], Q, QR) :- put(H, Q, Q1), putAll(T, Q1, QR).

% ---------- DRZEWA ---------- %

drzewo(empty).
drzewo(node(L, _, R)) :- drzewo(L), drzewo(R).

insertBST(empty, E, node(empty, E, empty)).
insertBST(node(L, E, R), E, node(L, E, R)).
insertBST(node(L, E1, R1), E2, node(L, E1, R2)) :- E1 < E2, insertBST(R1, E2, R2).
insertBST(node(L1, E1, R), E2, node(L2, E1, R)) :- E1 > E2, insertBST(L1, E2, L2).

deleteBST(empty, _, empty).
deleteBST(node(empty, E, empty), E, empty).
deleteBST(node(L, E, empty), E, L).
deleteBST(node(empty, E, R), E, R).
deleteBST(node(L, E, R1), E, node(L, H, R2)) :- wypiszBST(R1, [H|_]), deleteBST(R1, H, R2).
deleteBST(node(L, E1, R1), E2, node(L, E1, R2)) :- E1 < E2, deleteBST(R1, E2, R2).
deleteBST(node(L1, E1, R), E2, node(L2, E1, R)) :- E1 > E2, deleteBST(L1, E2, L2).

wypiszBST(empty) :- write('Drzewo puste').
wypiszBST(node(empty, E, empty)) :- write(E), write(' ').
wypiszBST(node(L, E, empty)) :- wypiszBST(L), write(E), write(' ').
wypiszBST(node(empty, E, R)) :- write(E), write(' '), wypiszBST(R).
wypiszBST(node(L, E, R)) :- wypiszBST(L), write(E), write(' '), wypiszBST(R).

wypiszBST(empty, []).
wypiszBST(node(L, E, R), Lista) :- wypiszBST(L, LL), wypiszBST(R, LR), append(LL, [E|LR], Lista).

stworzBST([], A, A).
stworzBST([H|T], A1, D) :- insertBST(A1, H, A2), stworzBST(T, A2, D).
stworzBST(L, D) :- stworzBST(L, empty, D).

sortBST(L, S) :- stworzBST(L, D), wypiszBST(D, S).

liscie(empty, A, A).
liscie(node(empty, E, empty), A, [E|A]).
liscie(node(L, _, R), A1, Lista) :- liscie(R, A1, A2), liscie(L, A2, Lista).
liscie(D, L) :- liscie(D, [], L).

wszerz(Q, A, A) :- empty(Q).
wszerz(Q, A, S) :- get(empty, Q, Q1), wszerz(Q1, A, S).
wszerz(Q, A, S) :-
    get(node(L, E, R), Q, Q1),
    put(L, Q1, Q2),
    put(R, Q2, Q3),
    wszerz(Q3, [E|A], S).
wszerz(D, S) :- init(Q0), put(D, Q0, Q), wszerz(Q, [], R), reverse(R, S).

% ---------- GRAFY ---------- %

connect(A, B) :- edge(A, B).
connect(A, B) :- edge(A, C), connect(C, B).

connect(Graf, A, B) :- member(kr(A, B), Graf).
connect(Graf, A, B) :- member(kr(A, C), Graf), connect(C, B).

path(A, B, [A, B]) :- edge(A, B).
path(A, B, [A|P]) :- edge(A, C), path(C, B, P).

pathC(A, B, Visited, [A, B]) :- edge(A, B), \+member(B, Visited).
pathC(A, B, Visited, [A|P]) :-
    edge(A, C),
    \+member(C, Visited),
    pathC(C, B, [C|Visited], P).
pathC(A, B, P) :- pathC(A, B, [], P).

remove(E, [E|T], T).
remove(E, [H|T1], [H|T2]) :- remove(E, T1, T2).

euler([kr(A, B)], [A, B]).
euler(Graf, [A, B|P]) :- remove(kr(A, B), Graf, Graf2), euler(Graf2, [B|P]).

% Znajdź wierzchołki osiągalne w grafie (możliwe cykle!) używając BFS.
sasiedzi(Graf, X, S) :- sasiedzi(Graf, X, [], S).
sasiedzi(Graf, X, A, S) :-
    (
        member(kr(X, Y), Graf), \+member(Y, A)
    ->  remove(kr(X, Y), Graf, Graf2),
        sasiedzi(Graf2, X, [Y|A], S)
    ;   S = A
    ).

osiagalne(_Graf, Q, A, A) :- empty(Q), !.
osiagalne(Graf, Q, A, L) :-
    get(X, Q, Q1),
    (
        member(X, A)
    ->  osiagalne(Graf, Q1, A, L)
    ;   sasiedzi(Graf, X, S),
        putAll(S, Q1, Q2),
        osiagalne(Graf, Q2, [X|A], L)
    ).
osiagalne(Graf, Start, L) :- init(Q0), put(Start, Q0, Q), osiagalne(Graf, Q, [], L).

% Odwrotna osiągalność: znajdź wierzchołki, z których osiągalny jest jakiś wierzchołek z podanej listy.
sasiedzi2(Graf, Y, S) :- sasiedzi2(Graf, Y, [], S).
sasiedzi2(Graf, Y, A, S) :-
    (
        member(kr(X, Y), Graf), \+member(X, A)
    ->  remove(kr(X, Y), Graf, Graf2),
        sasiedzi2(Graf2, Y, [X|A], S)
    ;   S = A
    ).

osiagalne2(_Graf, Q, A, A) :- empty(Q), !.
osiagalne2(Graf, Q, A, L) :-
    get(Y, Q, Q1),
    (
        member(Y, A)
    ->  osiagalne2(Graf, Q1, A, L)
    ;   sasiedzi2(Graf, Y, S),
        putAll(S, Q1, Q2),
        osiagalne2(Graf, Q2, [Y|A], L)
    ).
osiagalne2(Graf, End, L) :- init(Q0), put(End, Q0, Q), osiagalne2(Graf, Q, [], L).
