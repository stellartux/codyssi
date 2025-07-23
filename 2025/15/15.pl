#!/usr/bin/env swipl
% https://www.codyssi.com/view_problem_19

:- module(codyssi2025day15, []).
:- module(codyssi2025day15).
:- initialization(main, main).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(binary_trees).

line(artifact(ID, Code)) --> { length(Codes, 7) },
    nonblanks(Codes), { atom_codes(Code, Codes) }, " | ", integer(ID), eol.

load((Artifacts, Requests)) --> sequence(line, Artifacts0), eol, sequence(line, Requests), eos,
    { list_to_binary_tree(Artifacts0, Artifacts) } .
load(Filename, Result) :- once(phrase_from_file(load(Result), Filename)).

part_one_load --> load, part_one.
part_one((Artifacts, _), Result) :-
    aggregate_all(max(SumIDs) * max(Level), (
        group_by(Level, ID, bt_level_order(Artifacts, artifact(ID, _), Level), IDs),
        sum_list(IDs, SumIDs)
    ), Result0),
    Result is Result0.

part_two((Artifacts0, _), Result) :-
    LostArtifact = artifact(500000, _),
    bt_insert(Artifacts0, LostArtifact, Artifacts),
    bt_path(Artifacts, LostArtifact, Path),
    maplist(arg(2), Path, Codes),
    atomic_list_concat(Codes, -, Result).

part_three((Artifacts0, [R1, R2]), Result) :-
	bt_insert(Artifacts0, R1, Artifacts1),
    bt_insert(Artifacts1, R2, Artifacts),
    bt_path(Artifacts, R1, Path1),
    bt_path(Artifacts, R2, Path2),
    reverse(Path2, Path3),
    once((
        member(Artifact, Path3),
        memberchk(Artifact, Path1)
    )),
    arg(2, Artifact, Result).

main([Filename]) :-
    load(Filename, Input),
    time(part_one(Input, Result1)),
    writeln(Result1),
	time(part_two(Input, Result2)),
	writeln(Result2),
	time(part_three(Input, Result3)),
	writeln(Result3).
