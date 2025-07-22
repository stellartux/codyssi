#!/usr/bin/env swipl
% https://www.codyssi.com/view_problem_17

:- module(codyssi2025day13, []).
:- module(codyssi2025day13).
:- initialization(main, main).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(wgraphs, [
    ugraph_breadth_first_search/4,
    vertices_edges_to_wgraph/3,
    wgraph_breadth_first_search/4,
    wgraph_cycles/3,
    wgraph_to_ugraph/2
]).

load(Filename, WGraph) :-
    once(phrase_from_file(load(Edges), Filename)),
    vertices_edges_to_wgraph([], Edges, WGraph).

load(Edges) --> sequence(edge, Edges).

edge(A - (B - Weight)) --> node(A), " -> ", node(B), " | ", integer(Weight), eol.
node(N) --> nonblanks(N0), { N0 \== [], atom_codes(N, N0) }.

product_list(Xs, Product) :- foldl([A, B, C] >> (C is A * B), Xs, 1, Product).

part_one(WGraph, Result) :-
    wgraph_to_ugraph(WGraph, Graph),
    findall(PathLength, limit(3, order_by([desc(PathLength)], (
        ugraph_breadth_first_search(Graph, ['STT'], _, PathLength)
    ))), PathLengths),
    product_list(PathLengths, Result).

part_two(WGraph, Result) :-
    findall(PathLength, limit(3, order_by([desc(PathLength)], (
        wgraph_breadth_first_search(WGraph, ['STT'], _, PathLength)
    ))), PathLengths),
    product_list(PathLengths, Result).

part_three(WGraph, Result) :-
    aggregate_all(max(Length), wgraph_cycles(WGraph, _, Length), Result).

main([Filename]) :-
    load(Filename, WGraph),
    part_one(WGraph, Result1),
    format("~d~n", [Result1]),
	part_two(WGraph, Result2),
	format("~d~n", [Result2]),
	part_three(WGraph, Result3),
	format("~d~n", Result3).
