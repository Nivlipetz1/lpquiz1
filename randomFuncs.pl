/*  -------------------------------------- */

replace(W1, W2) :-
    replace_letters('A', 'B', W1, W2).

replace_letters(A,B,[A|Rest1], [B|Rest2]) :-
    replace_letters(A,B,Rest1,Rest2).
replace_letters(A,B,[B|Rest1], [A|Rest2]) :-
    replace_letters(A,B,Rest1,Rest2).
replace_letters(A,B,[H|Rest1], [H|Rest2]) :-
    H \= A, H \= B,
    replace_letters(A,B,Rest1,Rest2).
replace_letters(_, _, [], []).


/*  -------------------------------------- */

/*check distinct pairs of list predicate checkX where N is size of each pair*/
check_all_pairs(_, _, []).

check_all_pairs(N, H, [First|Rest]) :-
    checkX(N, H, First),
    check_all_pairs(N, H, Rest).

check_list(_, [])
check_list(N, [H|L]) :-
    check_all_pairs(N, H, L),
    check_list(N, L).

/*  -------------------------------------- */

% a function with mode (+N, -AllPerms) - returns all possible permutations of of length N DNA words
list_of_all_acgt_permutations(N, AllPerms) :-
    list_of_all_acgt_permutations(N, [['A'], ['C'], ['G'], ['T']], [], AllPerms).

% append all 4 possible chars ,which creates 4 new lists.
list_of_all_acgt_permutations(N, [H|T], BuildList, AllPerms) :-
    N > 1,
    append(['A'], H, L1),
    append(['C'], H, L2),
    append(['G'], H, L3),
    append(['T'], H, L4), %create all possible combinations of ACGT words
    list_of_all_acgt_permutations(N, T, [L1, L2, L3, L4|BuildList], AllPerms).  %add created combinations to list already built and keep applying recursively on the tail of posible permutations (T)

list_of_all_acgt_permutations(N, [], BuildList, AllPerms) :-                    %finished going throw all chars in [['A'], ['C'], ['G'], ['T']]
    N > 1,
    N1 is N - 1,                                                                %need this inorder to get all of the combinations of DNA words for length N
    list_of_all_acgt_permutations(N1, BuildList, [], AllPerms).

%stop condition when the above function reaches N=1
list_of_all_acgt_permutations(N, FinishedList, [], AllPerms) :-  
    N = 1,                                                                      %DNA words are now of length N that we received from the input of the function random_dna_word 
    FinishedList = AllPerms.

/*  -------------------------------------- */

create_list_size_n(N, [N | Acc], OriginalLen) :-
    N < OriginalLen,
    N1 is N + 1,
    create_list_size_n(N1, Acc, OriginalLen).

create_list_size_n(N, [N], N).

choose_k_from_n(K, N, [First|Rest]) :-
    first(K, N, First),
    last(K, N, Last),
    choose_k_from_n_aux(N, First, Last, Rest).
choose_k_from_n_aux(_, Last, Last, []).
choose_k_from_n_aux(N, Prev, Last, [Next|Rest]) :-
    increment(N, Prev, Next),
    choose_k_from_n_aux(N, Next, Last, Rest).
