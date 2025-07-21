#!/usr/bin/env swipl
% https://www.codyssi.com/view_problem_4

:- module(codyssi2024day4, []).
:- module(codyssi2024day4).
:- initialization(main, main).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(ugraphs)).

load(Graph) --> edges(Edges), { vertices_edges_to_ugraph([], Edges, Graph) }.
edges([A - B, B - A | Edges]) --> node(A), " <-> ", node(B), eol, edges(Edges).
edges([]) --> eol, eos.
node(N) --> nonblanks(N0), { atom_codes(N, N0) }.

load(Filename, Result) :- once(phrase_from_file(load(Result), Filename)).

part_one --> vertices, length.

search_depth_first(Graph, Here, There, Steps) :-
    search_depth_first(Graph, Here, There, 0, Steps).

search_depth_first(_, Here, Here, Steps, Steps).
search_depth_first(Graph0, Here, There, Steps0, Steps) :-
    Steps0 #< Steps,
    Steps1 #= Steps0 + 1,
    memberchk(Here - Theres, Graph0),
    del_vertices(Graph0, [Here], Graph),
    member(There0, Theres),
    search_depth_first(Graph, There0, There, Steps1, Steps).

part_two(Graph, Result) :-
    Steps #=< 3,
    aggregate_all(count, Location, search_depth_first(Graph, 'STT', Location, Steps), Result).

main([Filename]) :-
    load(Filename, Input),
    part_one(Input, Result1),
    writeln(Result1),
	part_two(Input, Result2),
	writeln(Result2).
