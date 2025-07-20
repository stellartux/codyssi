#!/usr/bin/env swipl
% https://www.codyssi.com/view_problem_3

:- module(codyssi2024day3, []).
:- module(codyssi2024day3).
:- initialization(main, main).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load(Result) --> sequence(line, Result), eol, eos.
line(format(A, B)) --> nonblanks(A), " ", integer(B), eol.
load(Filename, Result) :- once(phrase_from_file(load(Result), Filename)).

part_one(Input, Result) :-
    aggregate_all(sum(Base), member(format(_, Base), Input), Result).

to_base(format(Codes, 2), Number) :- foldl(binary, Codes, 0, Number).
to_base(format(Codes, 8), Number) :- foldl(octal, Codes, 0, Number).
to_base(format(Codes, 10), Number) :- foldl(decimal, Codes, 0, Number).
to_base(format(Codes, 16), Number) :- foldl(hexadecimal, Codes, 0, Number).

binary(Code, Number0, Number)   :- Number is  2 * Number0 + Code - 0'0.
octal(Code, Number0, Number)    :- Number is  8 * Number0 + Code - 0'0.
decimal(Code, Number0, Number)  :- Number is 10 * Number0 + Code - 0'0.

hexadecimal(Code0, Number0, Number) :-
    (   between(0'0, 0'9, Code0)
    ->  Digit = Code0 - 0'0
    ;   between(0'A, 0'F, Code0)
    ->  Digit = Code0 - 0'A + 10
    ;   between(0'a, 0'f, Code0)
    ->  Digit = Code0 - 0'a + 10
    ),
    Number is Number0 * 16 + Digit.

part_two --> maplist(to_base), sum_list.

base_65_digit(Code, Digit) :-
    once(nth0(Digit, `0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz!@#`, Code)).

base_65(0) --> "0".
base_65(N) --> { between(1, inf, N) }, base_65_(N).

base_65_(0) --> [].
base_65_(N) -->
    {   N > 0,
        divmod(N, 65, N0, Digit),
        base_65_digit(Code, Digit)
    },
    base_65_(N0),
    [Code].

to_base_65(Number, Codes) :-
    once(phrase(base_65(Number), Codes)).

part_three(Input, Result) :-
    ( is_list(Input) -> part_two(Input, Result0) ; number(Input), Result0 = Input ),
    to_base_65(Result0, Result).

main([Filename]) :-
    load(Filename, Input),
    time(part_one(Input, Result1)),
    writeln(Result1),
	time(part_two(Input, Result2)),
	writeln(Result2),
	time(part_three(Result2, Result3)),
	format("~s~n", [Result3]).
