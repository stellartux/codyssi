#!/usr/bin/env swipl
% https://www.codyssi.com/view_problem_7

:- module(codyssi2025day3, []).
:- module(codyssi2025day3).
:- initialization(main, main).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load(Result) --> sequence(range, Result), eol, eos.

range(Range) --> integer(A), "-", integer(B), blank, { Range in A..B }.

load(Filename, Result) :- once(phrase_from_file(load(Result), Filename)).

part_one(Ranges, Result) :-
    aggregate_all(sum(Size), (
        member(Range, Ranges),
        fd_size(Range, Size)
    ), Result).

unite_ranges(R1, R2, Range) :-
    fd_set(R1, FDSet1),
    fd_set(R2, FDSet2),
    fdset_union(FDSet1, FDSet2, FDSet),
    Range in_set FDSet.

pair_up(Range) --> [R1, R2], { unite_ranges(R1, R2, Range) }.

part_two(Ranges0, Result) :-
	once(phrase(sequence(pair_up, Ranges), Ranges0)),
    part_one(Ranges, Result).

pair_overlapping(Range), [R2]  --> [R1, R2], { unite_ranges(R1, R2, Range) }.

part_three(Ranges0, Result) :-
    once(phrase(sequence(pair_up, Ranges1), Ranges0)),
    once(phrase(sequence(pair_overlapping, Ranges), Ranges1, [_])),
    aggregate_all(max(Size), (
        member(Range, Ranges),
        fd_size(Range, Size)
    ), Result).

main([Filename]) :-
    load(Filename, Input),
    part_one(Input, Result1),
    writeln(Result1),
	part_two(Input, Result2),
	writeln(Result2),
	part_three(Input, Result3),
	writeln(Result3).
