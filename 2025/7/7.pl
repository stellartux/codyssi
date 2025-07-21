#!/usr/bin/env swipl
% https://www.codyssi.com/view_problem_11

:- module(codyssi2025day7, []).
:- module(codyssi2025day7).
:- initialization(main, main).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load((Tracks, Swaps, Test)) -->
    sequence(integer_line, Tracks), eol,
    sequence(swap_line, Swaps), eol,
    integer_line(Test), eol, eos.

integer_line(N)  --> integer(N), eol.
swap_line(xy(X, Y)) --> integer(X), "-", integer(Y), eol.

load(Filename, Result) :- once(phrase_from_file(load(Result), Filename)).

swap(xyz(X, Y, Z), List0, List) :-
    dif(Z, Y),
    dif(Z, X),
    dif(Y, X),
    nth1(Z, List0, C, List1),
    nth1(Z, List2, B, List1),
    nth1(Y, List2, B, List3),
    nth1(Y, List4, A, List3),
    nth1(X, List4, A, List5),
    nth1(X, List , C, List5).

swap(xy(X, Y), List0, List) :-
    dif(X, Y),
    nth1(X, List0, A, List1),
    nth1(X, List2, B, List1),
    nth1(Y, List2, B, List3),
    nth1(Y, List, A, List3).

part_one((Tracks0, Swaps, Test), Result) :-
    foldl(swap, Swaps, Tracks0, Tracks),
    nth1(Test, Tracks, Result).

triples(Pairs, xyz(X, Y, Z)) :-
    length(Pairs, Length),
    nth0(N0, Pairs, xy(X, Y)),
    N1 is (N0 + 1) mod Length,
    nth0(N1, Pairs, xy(Z, _)).

part_two((Tracks0, Swaps, Test), Result) :-
    findall(Triple, triples(Swaps, Triple), Triples),
    part_one((Tracks0, Triples, Test), Result).

swap_block(Length0, xy(X0, Y0)), Prefix, YBlock, Midfix, XBlock, Suffix -->
    {   (   X0 < Y0
        ->  X = X0, Y = Y0
        ;   X = Y0, Y = X0
        ),
        PrefixLength is X - 1,
        Length is min(Length0 - Y + 1, Y - X),
        MidfixLength is Y - (X + Length),
        length(Prefix, PrefixLength),
        length(XBlock, Length),
        length(Midfix, MidfixLength),
        length(YBlock, Length)
    },
    Prefix,
    XBlock,
    Midfix,
    YBlock,
    remainder(Suffix).

part_three((Tracks0, Swaps, Test), Result) :-
	length(Tracks0, Length),
    foldl(swap_block(Length), Swaps, Tracks0, Tracks),
    nth1(Test, Tracks, Result).

main([Filename]) :-
    load(Filename, Input),
    part_one(Input, Result1),
    writeln(Result1),
	part_two(Input, Result2),
	writeln(Result2),
	part_three(Input, Result3),
	writeln(Result3).
