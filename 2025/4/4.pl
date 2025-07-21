#!/usr/bin/env swipl
% https://www.codyssi.com/view_problem_8

:- module(codyssi2025day4, []).
:- module(codyssi2025day4).
:- initialization(main, main).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load(Messages) --> sequence(line, Messages), eol, eos.
line(Message) --> nonblanks(Message), eol, { Message \== [] }.

load(Filename, Result) :- once(phrase_from_file(load(Result), Filename)).

memory_required(Code, Memory0, Memory) :-
    (   between(0'A, 0'Z, Code)
    ->  Memory is Memory0 + Code - 0'A + 1
    ;   between(0'0, 0'9, Code)
    ->  Memory is Memory0 + Code - 0'0
    ).

memory_required(Message, Memory) :- foldl(memory_required, Message, 0, Memory).

part_one --> maplist(memory_required), sum_list.

compression(Message, Compressed) :-
    length(Message, Length0),
    Length1 is Length0 div 10,
    length(Prefix, Length1),
    prefix(Prefix, Message),
    length(Suffix, Length1),
    once(append(_, Suffix, Message)),
    Length is Length0 - 2 * Length1,
    once(phrase((Prefix, integer(Length), Suffix), Compressed)).

part_two --> maplist(compression), part_one.

repeated(N, Code) --> repeated(0, N, Code).
repeated(N0, N, Code) --> [Code], { N0 #< N, N1 #= N0 + 1 }, repeated(N1, N, Code).
repeated( N, N, _) --> [].

run_length_encoding([], []) :- !.

run_length_encoding(As0, Bs0) :-
    (   ground(Bs0)
    ->  once(phrase(integer(N), Bs0, [Code | Bs])),
        once(phrase(repeated(N, Code), As0, As))
    ;   ground(As0)
    ->  once(phrase(repeated(N, Code), As0, As)),
        once(phrase(integer(N), Bs0, [Code | Bs]))
    ),
    run_length_encoding(As, Bs).

part_three --> maplist(run_length_encoding), part_one.

main([Filename]) :-
    load(Filename, Input),
    part_one(Input, Result1),
    writeln(Result1),
	part_two(Input, Result2),
	writeln(Result2),
	part_three(Input, Result3),
	writeln(Result3).
