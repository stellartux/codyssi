#!/usr/bin/env swipl
% https://www.codyssi.com/view_problem_5

:- module(codyssi2025day1, []).
:- module(codyssi2025day1).
:- initialization(main, main).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

line(N) --> integer(N), eol.
offset(+) --> "+".
offset(-) --> "-".

sign(+, N, N).
sign(-, N0, N) :- N is -N0.

load((Numbers, Offsets)) --> sequence(line, Numbers), sequence(offset, Offsets), eol, eos.
load(Filename, Result) :- once(phrase_from_file(load(Result), Filename)).

part_one((Numbers0, Offsets), Result) :-
    maplist(sign, [+ | Offsets], Numbers0, Numbers),
    sum_list(Numbers, Result).

part_two((Numbers0, Offsets0), Result) :-
    reverse(Offsets0, Offsets),
    maplist(sign, [+ | Offsets], Numbers0, Numbers),
    sum_list(Numbers, Result).

pair_up(N) --> [A, B], { N is 10 * A + B }.

part_three((Numbers0, Offsets0), Result) :-
    reverse(Offsets0, Offsets1),
    once(phrase(sequence(pair_up, Numbers1), Numbers0)),
	same_length(Numbers1, Offsets),
    prefix(Offsets, [+ | Offsets1]),
    maplist(sign, Offsets, Numbers1, Numbers),
    sum_list(Numbers, Result).

main([Filename]) :-
    load(Filename, Input),
    part_one(Input, Result1),
    writeln(Result1),
	part_two(Input, Result2),
	writeln(Result2),
	part_three(Input, Result3),
	writeln(Result3).
