#!/usr/bin/env swipl
% https://www.codyssi.com/view_problem_15

:- module(codyssi2025day11, []).
:- module(codyssi2025day11).
:- initialization(main, main).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load(Result) --> sequence(line, Result), eos.
line((Number, Base)) --> nonblanks(Number), " ", integer(Base), eol.
load(Filename, Result) :- once(phrase_from_file(load(Result), Filename)).

digit_number( 0) --> "0".
digit_number( 1) --> "1".
digit_number( 2) --> "2".
digit_number( 3) --> "3".
digit_number( 4) --> "4".
digit_number( 5) --> "5".
digit_number( 6) --> "6".
digit_number( 7) --> "7".
digit_number( 8) --> "8".
digit_number( 9) --> "9".
digit_number(10) --> "A".
digit_number(11) --> "B".
digit_number(12) --> "C".
digit_number(13) --> "D".
digit_number(14) --> "E".
digit_number(15) --> "F".
digit_number(16) --> "G".
digit_number(17) --> "H".
digit_number(18) --> "I".
digit_number(19) --> "J".
digit_number(20) --> "K".
digit_number(21) --> "L".
digit_number(22) --> "M".
digit_number(23) --> "N".
digit_number(24) --> "O".
digit_number(25) --> "P".
digit_number(26) --> "Q".
digit_number(27) --> "R".
digit_number(28) --> "S".
digit_number(29) --> "T".
digit_number(30) --> "U".
digit_number(31) --> "V".
digit_number(32) --> "W".
digit_number(33) --> "X".
digit_number(34) --> "Y".
digit_number(35) --> "Z".
digit_number(36) --> "a".
digit_number(37) --> "b".
digit_number(38) --> "c".
digit_number(39) --> "d".
digit_number(40) --> "e".
digit_number(41) --> "f".
digit_number(42) --> "g".
digit_number(43) --> "h".
digit_number(44) --> "i".
digit_number(45) --> "j".
digit_number(46) --> "k".
digit_number(47) --> "l".
digit_number(48) --> "m".
digit_number(49) --> "n".
digit_number(50) --> "o".
digit_number(51) --> "p".
digit_number(52) --> "q".
digit_number(53) --> "r".
digit_number(54) --> "s".
digit_number(55) --> "t".
digit_number(56) --> "u".
digit_number(57) --> "v".
digit_number(58) --> "w".
digit_number(59) --> "x".
digit_number(60) --> "y".
digit_number(61) --> "z".
digit_number(62) --> "!".
digit_number(63) --> "@".
digit_number(64) --> "#".
digit_number(65) --> "$".
digit_number(66) --> "%".
digit_number(67) --> "^".

convert_digits(Base, Digit, Number0, Number) :- Number #= Number0 * Base + Digit.

convert((NumberCodes, Base), Result) :-
    once(phrase(sequence(digit_number, Digits), NumberCodes)),
    foldl(convert_digits(Base), Digits, 0, Result).

base_digits(Base, Number) --> { Number #>= 0, Number #< Base }, [Number].
base_digits(Base, Number) --> {
        Number #>= Base,
        Digit #= Number mod Base,
        Number0 #= Number div Base
    },
    base_digits(Base, Number0),
    [Digit].

to_base(Base, Number, Codes) :-
    once(phrase(base_digits(Base, Number), Digits)),
    once(phrase(sequence(digit_number, Digits), Codes)).

minimum_four_digit_base(Number, Base) :- Base is ceil(Number ^ 0.25).

part_one   --> max_list.
part_two   --> sum_list, to_base(68).
part_three --> sum_list, minimum_four_digit_base.

main([Filename]) :-
    load(Filename, Input),
    maplist(convert, Input, Numbers),
    part_one(Numbers, Result1),
    format("~d~n", [Result1]),
    sum_list(Numbers, Sum),
	to_base(68, Sum, Result2),
	format("~s~n", [Result2]),
	minimum_four_digit_base(Sum, Result3),
	format("~d~n", [Result3]).
