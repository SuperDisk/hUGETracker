% Bar parser and assembler for hUGETracker

/*
bar([n1]) :-
    X = 1,
    Y = 2,
    Z = 3.
*/

:- use_module(library(clpfd)).

find_shortest_premise_vars(Rules, X, Out) :-
    aggregate(min(Length, PVars), (
                  find_premise_vars(Rules, X, PVars),
                  PVars \= [X],
                  length(PVars, Length)
              ), min(_, Out)).

slice(L, S) :-
    length(L, N),
    between(1, N, M),
    length(S, M),
    append([_,S,_], L).

testData([1, 2, 3, 1, 2, 3, 1, 2, 3, 4, 4, 1, 2, 3]).

compressed([], _, []).

compressed(Ls, Subsection, Compressed) :-
    append(Subsection, RestLs, Ls),
    compressed(RestLs, Subsection, RestCompressed),
    Compressed = [lol | RestCompressed].

compressed(Ls, Subsection, Compressed) :-
    \+ append(Subsection, _, Ls),
    [X | Rest] = Ls,
    compressed(Rest, Subsection, RestCompressed),
    Compressed = [X | RestCompressed].

bestCompressed(Ls, Out) :-
    aggregate(min(Length, Subsection), (
                  slice(Ls, Subsection),
                  compressed(Ls, Subsection, Compressed),
                  length(Compressed, Length)),
              Out).

pattern(Ls, Patt, X) :-
    setof(Patt1, slice(Ls, Patt1), Patts),
    member(Patt, Patts),
    aggregate_all(count, slice(Ls, Patt), X),
    X #> 1.
