#!/usr/bin/env swipl
% https://www.codyssi.com/view_problem_10

:- module(codyssi2025day6, []).
:- module(codyssi2025day6).
:- initialization(main, main).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load(Result) --> string(Result), "\n".

load(Filename, Result) :- once(phrase_from_file(load(Result), Filename)).

part_one(Input, Result) :-
    aggregate_all(count, (
        member(Char, Input),
        is_alpha(Char)
    ), Result).

value(Char, Value) :-
    Char in 0'A..0'Z\/0'a..0'z,
    Value in 1..52,
    Char in 0'a..0'z #<==> Value #= Char - 0'a + 1,
    Char in 0'A..0'Z #<==> Value #= Char - 0'A + 27.

part_two(Input, Result) :-
	aggregate_all(sum(Value), (
        member(Char, Input),
        value(Char, Value)
    ), Result).

uncorrupt_(V0), [C2] --> [C0, C1], {
    value(C0, V0),
    (   is_alpha(C1)
    ->  C1 = C2
    ;   V2 #= (2 * V0 - 6) mod 52 + 1, value(C2, V2)
    )
}.

uncorrupt_(V) --> [C], { value(C, V) }.

uncorrupt(Vs) --> sequence(uncorrupt_, Vs).

part_three(Input, Result) :-
	once(phrase(uncorrupt(Uncorrupted), Input)),
    sum_list(Uncorrupted, Result).

main([Filename]) :-
    load(Filename, Input),
    part_one(Input, Result1),
    writeln(Result1),
	part_two(Input, Result2),
	writeln(Result2),
	part_three(Input, Result3),
	writeln(Result3).
