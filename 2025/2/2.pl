#!/usr/bin/env swipl
% https://www.codyssi.com/view_problem_6

:- module(codyssi2025day2, []).
:- module(codyssi2025day2).
:- initialization(main, main).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load(((C, B, A), Lines)) -->
    "Function A: ", function(A), eol,
    "Function B: ", function(B), eol,
    "Function C: ", function(C), eol,
    eol,
    sequence(line, Lines), eos.

function(plus(A)) --> "ADD ", integer(A).
function(times(A)) --> "MULTIPLY ", integer(A).
function(raised(A)) --> "RAISE TO THE POWER OF ", integer(A).
line(N) --> integer(N), eol.

load(Filename, Result) :- once(phrase_from_file(load(Result), Filename)).

times(A, B, C) :- C is B * A.
raised(A, B, C) :- C is B ^ A.

median(Numbers, Median) :-
    msort(Numbers, Sorted),
    length(Numbers, Length),
    Midpoint is Length div 2,
    nth0(Midpoint, Sorted, Median).

part_one((Formula, Qualities), MedianPrice) :-
    median(Qualities, MedianQuality),
    call_dcg(Formula, MedianQuality, MedianPrice).

part_two((Formula, Qualities), Price) :-
    aggregate_all(sum(Quality), (
        member(Quality, Qualities),
        Quality mod 2 =:= 0
    ), TotalQuality),
    call_dcg(Formula, TotalQuality, Price).

part_three((Formula, Qualities), MaxQuality) :-
    aggregate_all(max(Quality), (
        member(Quality, Qualities),
        call_dcg(Formula, Quality, Price),
        Price =< 15000000000000
    ), MaxQuality).

main([Filename]) :-
    load(Filename, Input),
    part_one(Input, Result1),
    writeln(Result1),
    part_two(Input, Result2),
    writeln(Result2),
    part_three(Input, Result3),
    writeln(Result3).
