#!/usr/bin/env swipl
% https://www.codyssi.com/view_problem_13

:- module(codyssi2025day9, []).
:- module(codyssi2025day9).
:- initialization(main, main).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load((Balances, Transactions)) -->
    sequence(load_balance, Balances0), eol,
    sequence(load_transaction, Transactions), eos,
    { sort(Balances0, Balances1), ord_list_to_rbtree(Balances1, Balances) }.

load_balance(Name - Balance) --> name(Name), " HAS ", integer(Balance), eol.
name(Name) --> nonblanks(Codes), { atom_codes(Name, Codes) }.
load_transaction(transaction(From, To, Amount)) -->
    "FROM ", name(From), " TO ", name(To), " AMT ", integer(Amount), eol.

load(Filename, Result) :- once(phrase_from_file(load(Result), Filename)).

calculate(Goal, (Balances0, Transactions), Result) :-
    foldl(Goal, Transactions, Balances0, Balances),
    aggregate_all(sum(Balance),
        limit(3, order_by([desc(Balance)], rb_in(_, Balance, Balances)))
    , Result).

transact1(transaction(From, To, Amount), Balances0, Balances) :-
    ToBalance #= ToBalance0 + Amount,
    FromBalance #= FromBalance0 - Amount,
    rb_update(Balances0, From, FromBalance0, FromBalance, Balances1),
    rb_update(Balances1, To, ToBalance0, ToBalance, Balances).

part_one --> calculate(transact1).

transact2(transaction(From, To, Amount), Balances0, Balances) :-
    ToBalance #= ToBalance0 + min(FromBalance0, Amount),
    FromBalance #= FromBalance0 - min(FromBalance0, Amount),
    rb_update(Balances0, From, FromBalance0, FromBalance, Balances1),
    rb_update(Balances1, To, ToBalance0, ToBalance, Balances).

part_two --> calculate(transact2).

add_debts(Balance, account(Balance, [])).

transact3(transaction(From, To, Amount), Accounts0, Accounts) :-
    rb_update(Accounts0, From, account(FromBalance0, FromDebts0), account(FromBalance, FromDebts), Accounts1),
    rb_update(Accounts1, To, account(ToBalance0, ToDebts), account(ToBalance, ToDebts), Accounts2),
    (   Amount =< FromBalance0
    ->  FromBalance #= FromBalance0 - Amount,
        ToBalance #= ToBalance0 + Amount,
        FromDebts0 = FromDebts
    ;   FromBalance = 0,
        ToBalance #= ToBalance0 + FromBalance0,
        Debt #= Amount - FromBalance0,
        append(FromDebts0, [To - Debt], FromDebts)
    ),
    clear_debts(To, Accounts2, Accounts).

clear_debts(Debtor, Accounts0, Accounts) :-
    (   rb_update(
            Accounts0,
            Debtor,
            account(Balance, [Creditor - Debt0 | Debts0]),
            account(Balance, Debts),
            Accounts1
        ),
        Balance > 0
    ->  (   Debt0 =< Balance
        ->  Debts = Debts0,
            transact3(transaction(Debtor, Creditor, Debt0), Accounts1, Accounts2),
            clear_debts(Debtor, Accounts2, Accounts)
        ;   Debt #= Debt0 - Balance,
            Debts = [Creditor - Debt | Debts0],
            transact3(transaction(Debtor, Creditor, Balance), Accounts1, Accounts)
        )
    ;   Accounts0 = Accounts
    ).

part_three((Balances0, Transactions), Result) :-
    rb_map(Balances0, add_debts, Balances1),
    foldl(transact3, Transactions, Balances1, Balances),
    aggregate_all(sum(Balance), (
        limit(3, order_by([desc(Balance)], rb_in(_, account(Balance, _), Balances)))
    ), Result).

main([Filename]) :-
    load(Filename, Input),
    part_one(Input, Result1),
    writeln(Result1),
	part_two(Input, Result2),
	writeln(Result2),
	part_three(Input, Result3),
	writeln(Result3).
