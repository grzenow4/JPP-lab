suma([], A, A).
suma([H|T], A, S) :- A1 is A + H, suma(T, A1, S).
suma(L, S) :- suma(L, 0, S).

dlugosc([], A, A).
dlugosc([_|T], A, K) :- A1 is A + 1, dlugosc(T, A1, K).
dlugosc(L, K) :- dlugosc(L, 0, K).

min([], A, A).
min([H|T], A, M) :- H < A, min(T, H, M).
min([_|T], A, M) :- min(T, A, M).
min([H|T], M) :- min(T, H, M).

odwroc([], A, A).
odwroc([H|T], A, R) :- odwroc(T, [H|A], R).
odwroc(L, R) :- odwroc(L, [], R).

palindrom([], A, A).
palindrom([H|T], A, S) :- palindrom(T, [H|A], S).
palindrom(S) :- palindrom(S, [], S).

slowo(S) :- slowo1(S, R), odwroc(S, R).
slowo1([a|T1], [b|T2]) :- slowo1(T1, T2).
slowo1(S, R) :- slowo2(S, R).
slowo2([b|T1], [a|T2]) :- slowo2(T1, T2).
slowo2([], []).

flagaPolska([], B, C, F) :- append(B, C, F).
flagaPolska([H|T], B, C, F) :- H == 'b', flagaPolska(T, [H|B], C, F).
flagaPolska([H|T], B, C, F) :- H == 'c', flagaPolska(T, B, [H|C], F).
flagaPolska(L, F) :- flagaPolska(L, [], [], F).

flagaHolenderska([], C, B, N, F) :- append(C, B, X), append(X, N, F).
flagaHolenderska([H|T], C, B, N, F) :- H == 'c', flagaHolenderska(T, [H|C], B, N, F).
flagaHolenderska([H|T], C, B, N, F) :- H == 'b', flagaHolenderska(T, C, [H|B], N, F).
flagaHolenderska([H|T], C, B, N, F) :- H == 'n', flagaHolenderska(T, C, B, [H|N], F).
flagaHolenderska(L, F) :- flagaHolenderska(L, [], [], [], F).

partition([], _, [], []).
partition([H|T], E, M, [H|W]) :- H > E, partition(T, E, M, W).
partition([H|T], E, [H|M], W) :- H =< E, partition(T, E, M, W).

qsort([], []).
qsort([H|T], S) :- partition(T, H, M, W), qsort(M, M1), qsort(W, W1), append(M1, [H|W1], S).

qsortA([], A, A).
qsortA([H|T], A, S) :- partition(T, H, M, W), qsortA(W, A, W1), qsortA(M, [H|W1], S).
qsortA(L, S) :- qsortA(L, [], S).

flatten([], A, A).
flatten([H|T], A, F) :- flatten(T, A, X), flatten(H, X, F).
flatten(E, A, [E|A]).
flatten(L, F) :- flatten(L, [], F).
