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
/*A*/

/*B*/

/*C*/

/*-------------------------------*/
/*task3*/
/*A*/

/*B*/

/*-------------------------------*/
/*task4*/
/*A*/

/*B*/

/*C*/