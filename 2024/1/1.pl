#!/usr/bin/env swipl

:- module(codyssi2024day1, []).
:- module(codyssi2024day1).
:- initialization(main, main).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load(Result) --> sequence(line, Result), eol, eos.
line(N)      --> integer(N), eol.

load(Filename, Result) :- once(phrase_from_file(load(Result), Filename)).

part_one(Input, Result) :- sum_list(Input, Result).

part_two(Vouchers, Input, Result) :-
    sort(0, @>=, Input, Sorted),
    length(Head, Vouchers),
    append(Head, Tail, Sorted),
    sum_list(Tail, Result).

part_three(Input, Result) :-
    aggregate_all(sum(Price * (2 * (N mod 2) - 1)), nth1(N, Input, Price), Result).

main([Filename]) :-
    load(Filename, Input),
    part_one(Input, Result1),
    writeln(Result1),
    ( sub_atom(Filename, _, _, _, 'example') -> Vouchers = 2 ; Vouchers = 20 ),
    part_two(Vouchers, Input, Result2),
    writeln(Result2),
    part_three(Input, Result3),
    writeln(Result3).
