#!/usr/bin/env swipl
% https://www.codyssi.com/view_problem_14

:- module(codyssi2025day10, []).
:- module(codyssi2025day10).
:- initialization(main, main).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load(Rows) --> sequence(row, Rows), eos.
row(Row) --> sequence(integer, " ", Row), eol, { Row \== [] }.
load(Filename, Result) :- once(phrase_from_file(load(Result), Filename)).

part_one(Rows, Result) :-
    transpose(Rows, Columns),
    aggregate_all(min(Danger), (
        ( member(List, Rows) ; member(List, Columns) ),
        sum_list(List, Danger)
    ), Result).

next_tile(yx(Y0, X), yx(Y, X)) :- succ(Y0, Y).
next_tile(yx(Y, X0), yx(Y, X)) :- succ(X0, X).

yxth0(yx(Y, X)) --> nth0(Y), nth0(X).

shortest_paths([[Head | Tail] | Rows], [Path | Paths]) :-
    scanl(plus, Tail, Head, Path),
    shortest_paths(Rows, Path, Paths).

shortest_paths([], _, []).
shortest_paths([Row | Rows], PathAbove, [Path | Paths]) :-
    min_row(Row, PathAbove, Path),
    shortest_paths(Rows, Path, Paths).

min_row([T1 | Row], [T0 | Prev], [T | Path]) :-
    T is T0 + T1,
    min_row(Row, T, Prev, Path).

min_row([T2 | Row], T1, [T0 | Prev], [T | Path]) :-
    T is T2 + min(T0, T1),
    min_row(Row, T, Prev, Path).

min_row([], _, [], []).

part_two   --> shortest_paths, nth1(15), nth1(15).
part_three --> shortest_paths, last, last.

main([Filename]) :-
    load(Filename, Grid),
    part_one(Grid, Result1),
    format("~d~n", [Result1]),
    shortest_paths(Grid, Paths),
    nth1(15, Paths, Row15),
    nth1(15, Row15, Result2),
	format("~d~n", [Result2]),
    last(Paths, LastRow),
    last(LastRow, Result3),
	format("~d~n", [Result3]).
