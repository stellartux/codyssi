#!/usr/bin/env swipl
% https://www.codyssi.com/view_problem_9

:- module(codyssi2025day5, []).
:- module(codyssi2025day5).
:- initialization(main, main).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load(Islands) --> sequence(line, Islands), eol, eos.

line(yx(Y, X)) --> "(", integer(X), ", ", integer(Y), ")", eol.

load(Filename, Result) :- once(phrase_from_file(load(Result), Filename)).

part_one(Islands, Result) :-
    aggregate_all(max(Distance) - min(Distance), (
        member(yx(Y, X), Islands),
        Distance is abs(Y) + abs(X)
    ), Result0),
    Result is Result0.

manhattan_distance(yx(Y, X), Distance) :- Distance is abs(Y) + abs(X).
manhattan_distance(yx(Y0, X0), yx(Y1, X1), Distance) :-
    Distance is abs(Y1 - Y0) + abs(X1 - X0).

compare_distance(C, YXA, YXB) :-
    manhattan_distance(YXA, DA),
    manhattan_distance(YXB, DB),
    compare(C0, DA, DB),
    (   C0 == (=)
    ->  arg(2, YXA, XA),
        arg(2, YXB, XB),
        compare(C1, XA, XB),
        (   C1 == (=)
        ->  arg(1, YXA, YA),
            arg(1, YXB, YB),
            compare(C, YA, YB)
        ;   C = C0
        )
    ;   C = C0
    ).

part_two(Islands0, Result) :-
	predsort(compare_distance, Islands0, [Island0 | Islands]),
    aggregate_all(min(Distance), (
        member(Island1, Islands),
        manhattan_distance(Island0, Island1, Distance)
    ), Result).

find_the_path([], _, Result, Result) :- !.

find_the_path(Islands0, Island0, Distance0, Distance) :-
    aggregate_all(min(D, Island1), (
        member(Island1, Islands0),
        manhattan_distance(Island0, Island1, D)
    ), min(Distance1, ClosestIsland)),
    once(select(ClosestIsland, Islands0, Islands)),
    Distance2 is Distance0 + Distance1,
    find_the_path(Islands, ClosestIsland, Distance2, Distance).

part_three(Islands, Result) :- find_the_path(Islands, yx(0, 0), 0, Result).

main([Filename]) :-
    load(Filename, Input),
    part_one(Input, Result1),
    writeln(Result1),
	part_two(Input, Result2),
	writeln(Result2),
	part_three(Input, Result3),
	writeln(Result3).
