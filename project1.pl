% 2048 in SWI-Prolog
% CPSC 312 2017W1
% Project 1
%
% To start a game, type
%  start.
%
% To start a self-playing game that uses a simple (not so good) algorithm, type
%  start_auto.
%


% start is true if a NxN board is created, on which two random numbers (2/4) are
% generated, from which a game is started.
%
start :-
  board(4, B),
  start(B).
start(B) :-
  welcome,
  new_number(B, B1),
  new_number(B1, B2),
  play(B2, 0).

% board(N, B) is true if B is a NxN matrix of zeroes, represented as a length N
% list of lists of length N.
%
% row(N, L) is true if L is a list of zeroes of length N.
%
board(N, B) :-
  board(N, N, B).
board(0, _, []).
board(R1, C, [H|T]) :-
  row(C, H),
  board(R0, C, T),
  R1 is R0+1.
row(0, []).
row(C1, [0|T]) :-
  row(C0, T),
  C1 is C0+1.

% welcome is true if a welcome message is displayed.
%
welcome :-
  write("Welcome to 2048! Slide the board by"), nl,
  write("entering an integer followed by a period."), nl.

% show_state(B, S) is true if the game state is printed to the console, showing
% board B with current score S.
%
show_state(B, S) :-
  nl,
  write("\u250F"), repeat("\u2501", 15), write("\u2533"), repeat("\u2501", 15), write("\u2513"), nl,
  write("\u2503       1       \u2503               \u2503"), nl,
  write("\u2503       \u2191       \u2503         SCORE \u2503"), nl,
  write("\u2503  4 \u2190     \u2192 2  \u2503               \u2503"), nl,
  write("\u2503       \u2193       \u2503"), writef("%14R \u2503", [S]), nl,
  write("\u2503       3       \u2503               \u2503"), nl,
  write("\u2517"), repeat("\u2501", 15), write("\u253B"), repeat("\u2501", 15), write("\u251B"), nl,
  display(B),
  write(" Score: "), write(S), nl, nl.

% play(B, S) is true if no moves are possible from board state B. Until then,
% moves are continuously read from user input.
%
play(B, S) :-
  lose(B),
  show_state(B, S),
  write("You lose!"), nl, nl,
  abort.
play(B1, S1) :-
  show_state(B1, S1),
  read(D), nl,
  slide(B1, D, B2, S2, S1),
  new_number(B2, B3),
  S3 is S1+S2,
  play(B3, S3).

% lose(B) is true if no move is possible that will free up a square in B
%
lose(B) :-
  slide_right(B, B, _),
  slide_left(B, B, _),
  slide_up(B, B, _),
  slide_down(B, B, _).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                              %
%                               Performing moves                               %
%                                                                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% slide(B1, N, B2, S, C) is true if B2 is the result of sliding game board B1 in
% one of the four cardinal directions specified by integer N, defined as follows:
%  up:    1
%  right: 2
%  down:  3
%  left:  4
%
% where S is the score obtained from performing such a move, and C is the
% current score.
%
slide(B1, 1, B2, S, _) :-
  dif(B1, B2),
  slide_up(B1, B2, S).
slide(B1, 2, B2, S, _) :-
  dif(B1, B2),
  slide_right(B1, B2, S).
slide(B1, 3, B2, S, _) :-
  dif(B1, B2),
  slide_down(B1, B2, S).
slide(B1, 4, B2, S, _) :-
  dif(B1, B2),
  slide_left(B1, B2, S).
slide(B, _, B, _, S) :-
  play(B, S).

% slide_right(B1, B2, S) is true if B2 is the result of sliding board (matrix) B1
% to the right, collapsing all elements to the right according to the standard
% 2048 game rules, and S is the number of points obtained from this move.
%
% slide_left/up/down accomplish the same thing in the other three directions by
% using slide_right after a sequence of matrix flip and transpose operations.
%
slide_right([], [], 0).
slide_right([H|T1], [R|T2], S1) :-
  collapse_right(H, R, S2),
  slide_right(T1, T2, S3),
  S1 is S2+S3.
slide_left(B1, B4, S) :-
  flip(B1, B2),
  slide_right(B2, B3, S),
  flip(B3, B4).
slide_up(B1, B2, S) :-
  transpose(B1, B1T),
  slide_left(B1T, B2T, S),
  transpose(B2T, B2).
slide_down(B1, B2, S) :-
  transpose(B1, B1T),
  slide_right(B1T, B2T, S),
  transpose(B2T, B2).

% collapse_right(L1, L2, S) is true if L2 is the result of moving all nonzero
% elements of list L1 as far as possible to the right, combining pairs of equal
% elements into their sum, and S is the number of points obtained from this move.
%
% sink_right(E, L1, L2, S) is true if L2 is the result of sinking nonzero element E
% into list L1 as far right as possible, until it reaches a nonzero element,
% combining with it if they are equal, and S is the number of points obtained
% from this move.
%
collapse_right([], [], 0).
collapse_right([0|T1], [0|T2], S) :-
  collapse_right(T1, T2, S).
collapse_right([H|T1], T3, S1) :-
  dif(H, 0),
  collapse_right(T1, T2, S2),
  sink_right(H, T2, T3, S3),
  S1 is S2+S3.
sink_right(E, [], [E], 0).
sink_right(E, [H|T], [E,H|T], 0) :-
  dif(E, H),
  dif(H, 0).
sink_right(E, [E|T], [0,EE|T], EE) :-
  EE is E+E.
sink_right(E, [0|T], [0|R], S) :-
  sink_right(E, T, R, S).

% new_number(B1, B2) is true if B2 is the result of placing a 2 or 4 (chosen
% randomly with probabilities 0.9 and 0.1 respectively) into the game board B1
% at a random location containing a 0.
%
new_number(B1, B2) :-
  zeroes(B1, NZ),
  size(NZ, N),
  random_between(1, N, NR),
  nth(NZ,NR,[R,C]),
  random_between(1, 10, NR2),
  random_tile(NR2, NR3),
  replace_m(NR3,R,C,B1,B2).
random_tile(1, 4).
random_tile(N, 2) :-
  N>1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                              %
%                   Helpers for list and matrix manipulation                   %
%                                                                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% flip(M1, M2) is true if M2 is the result of flipping matrix M1 along
% the y-axis.
%
% reverse(L1, L2) is true if L2 is the result of reversing list L1.
%
flip([], []).
flip([H|T1], [R|T2]) :-
  reverse(H, R),
  flip(T1, T2).
reverse(H, R) :-
  reverse(H, [], R).
reverse([], R, R).
reverse([H|T], A, R) :-
  reverse(T, [H|A], R).

% transpose(M1, M2) is true if M2 is the transpose of matrix M1.
% From the old SWI-Prolog library.
% SOURCE: https://stackoverflow.com/questions/4280986
%
transpose([], []).
transpose([F|Fs], Ts) :-
  transpose(F, [F|Fs], Ts).
transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
  lists_firsts_rests(Ms, Ts, Ms1),
  transpose(Rs, Ms1, Tss).
lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
  lists_firsts_rests(Rest, Fs, Oss).

% size(L, N) is true if N is the number of elements in list L.
%
size([], 0).
size([_|T], N) :-
  size(T, N0),
  N is N0+1.

% nth(L, N, E) is true if E is the Nth element of list L (1-indexed).
%
nth([H|_], 1, H).
nth([_|T], N1, E) :-
  nth(T, N0, E),
  N1 is N0+1.

% zeroes(M, L) is true if L is a list of coordinates [R, C] of the zero
% elements of matrix M.
%
% zeroes_row(H, L, R) is true if L is a list of coordinates [R, C] of the zero
% elements of list H given row R.
%
zeroes(M, L) :-
  size(M, N0),
  N1 is N0+1,
  zeroes(M, L, N1, _).
zeroes([], [], N, N).
zeroes([H|T], L0, N, R0) :-
  zeroes_row(H, L1, R0),
  zeroes(T, L2, N, R1),
  append(L1, L2, L0),
  R0 is R1-1.
zeroes_row(H, L, R) :-
  size(H, N0),
  N1 is N0+1,
  zeroes_row(H, L, R, N1, _).
zeroes_row([], [], _, N, N).
zeroes_row([0|T], [[R,C0]|L1], R, N, C0) :-
  zeroes_row(T, L1, R, N, C1),
  C0 is C1-1.
zeroes_row([H|T], L1, R, N, C0) :-
  dif(H, 0),
  zeroes_row(T, L1, R, N, C1),
  C0 is C1-1.

% replace_m(E,R,I,M1,M2) is true if M2 is the result of replacing the
% Ith element in row R of matrix M1 with E (1-indexed).
%
% replace_l(E,I,L,R) is true if R is the result of replacing the
% Ith element in list L with E (1-indexed).
%
replace_m(E, 1, I, [M1H|T], [M2H|T]) :-
  replace_l(E, I, M1H, M2H).
replace_m(E, R1, I, [H|M1T], [H|M2T]) :-
  replace_m(E, R0, I, M1T, M2T),
  R1 is R0+1.
replace_l(E, 1, [_|T], [E|T]).
replace_l(E, I1, [H|T], [H|R]) :-
  replace_l(E, I0, T, R), I1 is I0+1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                              %
%                    Helpers for displaying the game board                     %
%                                                                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% display(B) is true if the game board (matrix) B is written to the console.
%
% display_row(L) is true if list L is is written to the console, with each
% element padded to 5 spaces.
%
% top/middle/bottom/side_border(B) are true if borders are printed corresponding
% to a grid matching the size of board B.
%
display(B) :-
  top_border(B),
  display_helper(B).
display_helper([H]) :-
  side_border(H),
  display_row(H),
  side_border(H),
  bottom_border(H).
display_helper([H|T]) :-
  side_border(H),
  display_row(H),
  side_border(H),
  middle_border(H),
  display_helper(T).

display_row([]) :-
  write("\u2503"),
  nl.
display_row([H|T]) :-
  dif(H,0),
  write("\u2503"),
  writef("%6R ", [H]),
  display_row(T).
display_row([0|T]) :-
  write("\u2503"),
  repeat(" ", 7),
  display_row(T).

top_border(B) :-
  write("\u250F"),
  top_border_helper(B).
top_border_helper([_]) :-
  repeat("\u2501", 7),
  write("\u2513"),
  nl.
top_border_helper([_|T]) :-
  repeat("\u2501", 7),
  write("\u2533"),
  top_border_helper(T).

middle_border(B) :-
  write("\u2523"),
  middle_border_helper(B).
middle_border_helper([_]) :-
  repeat("\u2501", 7),
  write("\u252B"),
  nl.
middle_border_helper([_|T]) :-
  repeat("\u2501", 7),
  write("\u254B"),
  middle_border_helper(T).

bottom_border(B) :-
  write("\u2517"),
  bottom_border_helper(B).
bottom_border_helper([_]) :-
  repeat("\u2501", 7),
  write("\u251B"),
  nl.
bottom_border_helper([_|T]) :-
  repeat("\u2501", 7),
  write("\u253B"),
  bottom_border_helper(T).

side_border([]) :-
  write("\u2503"),
  nl.
side_border([_|T]) :-
  write("\u2503"),
  repeat(" ", 7),
  side_border(T).

% repeat(S, N) is true if string S is written to the console N times
%
repeat(_, 0).
repeat(S, N1) :-
  write(S),
  repeat(S, N0),
  N1 is N0+1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                              %
%                                      AI                                      %
%                                                                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% A discussion of possible algorithms:
%  https://stackoverflow.com/questions/22342854
%
% Some of the algorithms are rather complicated, so we chose one of the simpler
% approaches. Here we use a multi-depth exhaustive search algorithm that favours
% empty squares, performing the move which maximizes the maximum number of empty
% squares at any step after performing D moves (for search depth D). Since there
% are 4 possible moves per step, and at each step we need to count over N^2
% tiles for a NxN board, the runtime per move is O(4^D * N^2).
%
% This algorithm actually does not work very well because this version of the
% game (which is the official version) is not deterministic. The algorithm
% assumes that its prediction of the position and size of new tiles will be
% correct, when in fact they often are not. This is one of the main drawbacks
% of this approach, and possibly a reason why increasing the search depth does
% not improve performance (but rather worsens it, actually).
%

% start_auto, play_auto, and slide_auto are slightly adjusted versions of start,
% play, and slide that have been modified to work with the AI.
%
start_auto :-
  board(4, B),
  start_auto(B).
start_auto(B) :-
  new_number(B, B1),
  new_number(B1, B2),
  play_auto(B2, 0).

play_auto(B, S) :-
  lose(B),
  show_state(B, S),
  write("You lose!"), nl, nl,
  abort.
play_auto(B1, S1) :-
  show_state(B1, S1), nl,
  %% first_valid_move(B1, [1,2,3,4], D),
  optimal_move(B1, 2, D),
  slide_auto(B1, D, B2, S2),
  new_number(B2, B3),
  S3 is S1+S2,
  play_auto(B3, S3).

slide_auto(B1, 1, B2, S) :-
  slide_up(B1, B2, S).
slide_auto(B1, 2, B2, S) :-
  slide_right(B1, B2, S).
slide_auto(B1, 3, B2, S) :-
  slide_down(B1, B2, S).
slide_auto(B1, 4, B2, S) :-
  slide_left(B1, B2, S).

% optimal_move(B, D, N) is true if N is the integer representation of the
% direction of the 'optimal' move on board B, given a search depth of D.
% We define an optimal move being the first of a sequence of D moves which has
% the largest value of the maximum number of empty tiles at any step of each
% sequence of D moves.
%
% max_score(B, D, Z, N) is true if Z is the largest score obtained after
% performing move N on board B and then any combination of D-1 further moves.
%
optimal_move(B, D1, N) :-
  D0 is D1-1,
  max_score(B, D0, Z1, 1),
  max_score(B, D0, Z2, 2),
  max_score(B, D0, Z3, 3),
  max_score(B, D0, Z4, 4),
  generate_moves([Z1, Z2, Z3, Z4], L),
  first_valid_move(B, L, N).
max_score(B1, 0, Z, N) :-
  slide_auto(B1, N, B2, _),
  zeroes_count(B2, Z).
max_score(B1, D1, Z, N) :-
  D0 is D1-1,
  slide_auto(B1, N, B2, _),
  zeroes_count(B2, Z5),
  max_score(B2, D0, Z1, 1),
  max_score(B2, D0, Z2, 2),
  max_score(B2, D0, Z3, 3),
  max_score(B2, D0, Z4, 4),
  max([Z1,Z2,Z3,Z4,Z5], Z).

% generate_moves(L, M) is true if M is a sorted list of moves, from best to
% worst, given a list of scores L (number of empty tiles obtained by
% performing each move).
%
generate_moves(L, []) :-
  max_pos(L, -1).
generate_moves(L, [N|T]) :-
  dif(L,[]),
  max_pos(L, N),
  replace_l(-1.0Inf, N, L, R),
  generate_moves(R, T).

% Given a list of possible moves L, first_valid_move(B, L, N) is true if N is
% the first non-trivial move on board B.
%
first_valid_move(B1, [N|_], N) :-
  slide_auto(B1, N, B2, _),
  dif(B1, B2).
first_valid_move(B, [N1|T], N2) :-
  slide_auto(B, N1, B, _),
  first_valid_move(B, T, N2).

% max(L, E) is true if E is the largest element in L
%
max(L, E) :-
  max(L, -1.0Inf, E).
max([], E, E).
max([H|T], A, E) :-
  H>A,
  max(T, H, E).
max([H|T], A, E) :-
  H=<A,
  max(T, A, E).

% max_pos(L, I) is true if I is the position of the largest element in L
% (1-indexed)
%
max_pos(L, I) :-
  max_pos(L, -1.0Inf, 1, -1, I).
max_pos([], _, _, C, C).
max_pos([H|T], A, P0, _, I) :-
  H>A,
  P1 is P0+1,
  max_pos(T, H, P1, P0, I).
max_pos([H|T], A, P0, C, I) :-
  H=<A,
  P1 is P0+1,
  max_pos(T, A, P1, C, I).

% zeroes_count(M, N) is true if N is the number of zero tiles in matrix M.
%
% zeroes_count_row(L, N) is true if N is the number of zero tiles in list L.
%
zeroes_count([], 0).
zeroes_count([H|T], N2) :-
  zeroes_count_row(H, N0),
  zeroes_count(T, N1),
  N2 is N0+N1.
zeroes_count_row([], 0).
zeroes_count_row([0|T], N1) :-
  zeroes_count_row(T, N0),
  N1 is N0+1.
zeroes_count_row([H|T], N) :-
  dif(H, 0),
  zeroes_count_row(T, N).