:- use_module(naive_sat).

/*task1 - nanogram line*/
/*A*/
place_block(Block, N, [X|Xs], ReducedN, ReducedXs) :-
    X = 1,
    Block1 is Block - 1,
    N1 is N - 1,
    place_block(Block1, N1, Xs, ReducedN, ReducedXs).
place_block(0,N,[0|Xs], ReducedN, ReducedXs) :-
    ReducedXs = Xs,
    N1 is N - 1,
    N1 = ReducedN.

place_block(0,0,[],0,[]).

nonogram_verify([], 0, []).

nonogram_verify(Ns,N,Xs) :-
    length(Xs, N),
    nonogram_verify_elements(Ns, N, Xs).

nonogram_verify_elements([Block|Ns], N, Xs) :-
    Block > 0,
    place_block(Block, N, Xs, ReducedN, ReducedXs),
    nonogram_verify_elements(Ns, ReducedN, ReducedXs).

nonogram_verify_elements([Block|Ns], N, [X|Xs]) :-
    Block > 0,
    X = 0,
    N1 is N - 1,
    nonogram_verify_elements([Block|Ns], N1, Xs).

nonogram_verify_elements([Block|Ns],N,[X|Xs]) :-
    Block = 0,
    X = 0,
    N1 is N - 1,
    nonogram_verify_elements(Ns, N1, Xs).

nonogram_verify_elements([],N,[X|Xs]) :-
    N > 0,
    X = 0,
    N1 is N - 1,
    nonogram_verify_elements([], N1, Xs).    
nonogram_verify_elements([], 0, []).

/*B*/
nonogram(Ns, N, Xs) :-
    length(Xs, N),
    build_nonogram(N, Xs),
    nonogram_verify(Ns, N, Xs).

build_nonogram(N, [1|Xs]) :-
    N1 is N - 1,
    build_nonogram(N1, Xs).

build_nonogram(N, [0|Xs]) :-
    N1 is N - 1,
    build_nonogram(N1, Xs).

build_nonogram(0, []).

/*-------------------------------*/
/*task2*/
bin_gen(Xs, N) :-
    direct(Xs, N, CNF),
    sat(CNF).
/*A*/

direct(Xs, N, CNF) :-
    length(Xs, N),
    build_not_more_than_one_cnf(Xs, CNF1),  % for each i, (NOT Ci OR NOT Cj)
    build_atleast_one_cnf(Xs, CNF2), %Ci OR until Cn
    append(CNF1, CNF2, CNF).

build_not_more_than_one_cnf([X1, X2 | Rest], CNF) :-
    build_not_more_than_one_cnf([X1 | Rest], CNF1),
    build_not_more_than_one_cnf([X2 | Rest], CNF2),
    append([[[-X1, -X2]], CNF1, CNF2], CNF).

build_not_more_than_one_cnf([], []).
build_not_more_than_one_cnf([_], []).

build_atleast_one_cnf(Xs, CNF):-
    append([[Xs]], CNF).
/*B*/

diff(Xs,Ys,Cnf) :-
    length(Xs, N),
    direct(Xs, N, CNFX),
    direct(Ys, N, CNFY),
    build_both_not_one_cnf(Xs, Ys, CNFXY),
    append([CNFX, CNFY, CNFXY], CNF).

build_both_not_one_cnf([X|Xs], [Y|Ys], CNF) :-
    build_both_not_one_cnf(Xs, Ys, CNF1),
    append([[[-X, -Y]], CNF1], CNF).

build_both_not_one_cnf([],[],[]).

/*C*/
allDiff([X,Y|XXs],N,CNF) :-
    diff(X, Y, CNFDIFF),
    allDiff(X, N, CNFRECURSIONX),
    allDiff(Y, N, CNFRECURSIONY),
    append([CNFDIFF, CNFRECURSIONX, CNFRECURSIONY], CNF).

allDiff([],_,[]).


/*-------------------------------*/
/*task3*/
/*A*/

/*B*/

/*-------------------------------*/
/*task4*/
/*A*/
bit_vector(N, Vector) :-
    create_vector(N, Vector).

create_vector(N, [0|Vector]) :-
    N > 0,
    N1 is N - 1,
    create_vector(N1, Vector).

create_vector(N, [1|Vector]) :-
    N > 0,
    N1 is N - 1,
    create_vector(N1, Vector).

create_vector(0, []).

/*B*/
% apply_network(Cs,In,Out) :-
%     apply_network(Cs).

% apply_network([comparator(In1,In2,In1,In2)|RestComparators]) :-
%     In1 <= In2,
%     apply_network(RestComparators).

% apply_network([comparator(In1,In2,In2,In1)|RestComparators]) :-
%     In2 > In1,
%     apply_network(RestComparators).

/*C*/