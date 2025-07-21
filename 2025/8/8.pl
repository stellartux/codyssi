#!/usr/bin/env swipl
% https://www.codyssi.com/view_problem_12

:- module(codyssi2025day8, []).
:- module(codyssi2025day8).
:- initialization(main, main).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load(Lines) --> sequence(line, Lines), eos.
line(Line)  --> nonblanks(Line), eol, { Line \== [] }.
load(Filename, Result) :- once(phrase_from_file(load(Result), Filename)).

part_one(Lines, Result) :-
    aggregate_all(count, (
        member(Line, Lines),
        member(Char, Line),
        is_alpha(Char)
    ), Result).

fixed_point(Goal, S0, S) :-
    call(Goal, S0, S1),
    (   S0 == S1
    ->  S1 = S
    ;   fixed_point(Goal, S1, S)
    ).

is_replaceable(C) :- once( is_alpha(C) ; C = 0'- ).

replace(Cs) --> [C0, C1],
    {   is_digit(C0), is_replaceable(C1)
    ;   is_digit(C1), is_replaceable(C0)
    },
    replace(Cs).

replace([C | Cs]) --> [C], replace(Cs).

replace([]) --> [].

replace(Line, Reduced) :- once(phrase(replace(Reduced), Line)).

part_two(Lines, Result) :-
    aggregate_all(sum(Length), (
        member(Line, Lines),
        fixed_point(replace, Line, Reduced),
        length(Reduced, Length)
    ), Result).

replace2(Cs) --> [C0, C1],
    {   is_digit(C0), is_alpha(C1)
    ;   is_digit(C1), is_alpha(C0)
    },
    replace2(Cs).

replace2([C | Cs]) --> [C], replace2(Cs).

replace2([]) --> [].

replace2(Line, Reduced) :- once(phrase(replace2(Reduced), Line)).

part_three(Lines, Result) :-
    aggregate_all(sum(Length), (
        member(Line, Lines),
        fixed_point(replace2, Line, Reduced),
        length(Reduced, Length)
    ), Result).

main([Filename]) :-
    load(Filename, Input),
    part_one(Input, Result1),
    writeln(Result1),
	part_two(Input, Result2),
	writeln(Result2),
	part_three(Input, Result3),
	writeln(Result3).
