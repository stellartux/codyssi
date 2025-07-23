#!/usr/bin/env swipl
% https://www.codyssi.com/view_problem_18

:- module(codyssi2025day14, []).
:- module(codyssi2025day14).
:- initialization(main, main).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load(Filename, Result) :- once(phrase_from_file(load(Result), Filename)).
load(Result) --> sequence(line, Result), eol, eos, { Result \== [] }.

line(item(ItemCode, Quality, Cost, UniqueMaterials)) -->
    integer(_), " ", item_code(ItemCode), " | Quality : ", integer(Quality),
    ", Cost : ", integer(Cost), ", Unique Materials : ", integer(UniqueMaterials), eol.

item_code(ItemCode) --> nonblanks(Codes), { Codes \== [], atom_codes(ItemCode, Codes) }.

prefix_n(Prefix, N, List) :- length(Prefix, N), prefix(Prefix, List).

sum_items(Items, Totals) :-
    aggregate_all(total(sum(Quality), sum(Cost), sum(UniqueMaterials)), (
        member(item(_, Quality, Cost, UniqueMaterials), Items)
    ), Totals).

part_one --> part_one(5).
part_one(N, Items, Result) :-
    sort(3, >=, Items, ItemsSortedByCost),
    sort(2, >=, ItemsSortedByCost, ItemsSortedByQualityThenCost),
    prefix_n(Top, N, ItemsSortedByQualityThenCost),
    sum_items(Top, total(_, _, Result)).


optimal_combination(MaxCost, Items, Result) :-
    sort(3, =<, Items, SortedByCost),
    once(order_by([desc(Quality), asc(UniqueMaterials)], combinations(SortedByCost, 0, MaxCost, 0, Quality, 0, UniqueMaterials))),
    Result is Quality * UniqueMaterials.

combinations([item(_, Q1, C1, UM1) | Items], C0, MaxCost, Q0, Quality, UM0, UniqueMaterials ) :-
    C2 #= C0 + C1,
    Q2 #= Q0 + Q1,
    UM2 #= UM0 + UM1,
    C2 #=< MaxCost,
    (   Q2 = Quality, UM2 = UniqueMaterials
    ;   combinations(Items, C2, MaxCost, Q2, Quality, UM2, UniqueMaterials)
    ;   combinations(Items, C0, MaxCost, Q0, Quality, UM0, UniqueMaterials)
    ).

part_two   --> optimal_combination(30).

main([Filename]) :-
    load(Filename, Input),
    part_one(Input, Result1),
    format("~d~n", [Result1]),
	time(part_two(Input, Result2)),
	format("~d~n", [Result2]).
