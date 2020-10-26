/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 Prepare training examples
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
%% Use jurassic to load json data
:- use_module(library(http/json)).

%% Primitives for examples
:- dynamic ex/4.

%% examples of triadic sum
ex_sum(X,Y,Z) :-
    ex([X,Y], Z, _, _).

%% examples of triadic sum with reverse binary codes
ex_sum_rev(X,Y,Z) :-
    ex_bin_rev([X,Y], Z).

%% Examples in binary code, reversed
ex_bin_rev(X, Y) :-
    ex(X1, Y1, _, _),
    %% element_sharp(X1, X2),
    element_reverse(X1, X),
    reverse(Y1, Y).

%% Examples in decimal code, reversed
ex_dec_rev(X, Y) :-
    ex(_, _, X1, Y1),
    element_num2list(X1, X2),
    num_to_list(Y1, Y2),
    element_reverse(X2, X),
    reverse(Y2, Y).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 Loading data
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
file_path('nat_small_pair_sum.json').

assert_all_examples([]).
assert_all_examples([E|Es]) :-
    assertz(ex(E.'X_bin',
               E.'Y_bin',
               E.'X',
               E.'Y')),
    assert_all_examples(Es).

:-  file_path(Path),
    open(Path, read, File),
    json_read_dict(File, Dict),
    assert_all_examples(Dict.data),
    close(File), !.


%% Convert decimal number to digit list
num_to_list(N, L) :-
    number_chars(N, Cs),
    maplist(atom_number, Cs, L).

%% Convert list of decimal numbers to list of digital lists
element_num2list([], []) :- !.
element_num2list([N|Ns], [L|Ls]) :-
    num_to_list(N, L),
    element_num2list(Ns, Ls).

%% Reverse lists in list
element_reverse([], []) :- !.
element_reverse([E|Es], [R|Rs]) :-
    reverse(E, R),
    element_reverse(Es, Rs).

%% add '#' to the end
element_sharp([], []) :- !.
element_sharp([E|Es], [R|Rs]) :-
    append([#], E, R),
    element_sharp(Es, Rs).


x:-
    forall(ex_sum(X,Y,Z),writeln(pos(sum(X,Y,Z)))).