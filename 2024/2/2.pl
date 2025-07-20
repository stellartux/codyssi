#!/usr/bin/env swipl
% https://www.codyssi.com/view_problem_2

:- module(codyssi2024day2, []).
:- module(codyssi2024day2).
:- initialization(main, main).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load(Result) --> sequence(line, Result), eol, eos.
line(1) --> "TRUE",  eol.
line(0) --> "FALSE", eol.

load(Filename, Result) :- once(phrase_from_file(load(Result), Filename)).

part_one(Sensors, Sum) :- aggregate_all(sum(ID), nth1(ID, Sensors, 1), Sum).

gates([], []).
gates([S1, S2], [A]) :- A #= S1 /\ S2.
gates([S1, S2, S3, S4 | Sensors], [A, B | Gated]) :-
    A #= S1 /\ S2,
    B #= S3 \/ S4,
    gates(Sensors, Gated).

part_two --> gates, sum_list.

layered_gates([Result], Result) :- !.
layered_gates(Sensors, Result) :-
    Result #= Result0 + Result1,
    sum_list(Sensors, Result0),
    gates(Sensors, Gates),
    layered_gates(Gates, Result1).

part_three --> layered_gates.

main([Filename]) :-
    load(Filename, Input),
    time(part_one(Input, Result1)),
    writeln(Result1),
	time(part_two(Input, Result2)),
	writeln(Result2),
	time(part_three(Input, Result3)),
	writeln(Result3).
