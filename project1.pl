% 2048 in SWI-Prolog
% CPSC 312 2017W1
% Project 1
%
% To start a game, type
%  start.
%

start :-
  board(4, B),
  start(B).
start(B) :-
  welcome,
  new_number(B, B1),
  new_number(B1, B2),
  play(B2).

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

welcome :-
  write("Welcome to 2048! Slide the board by"), nl,
  write("entering an integer followed by a period."), nl.

show_state(B) :-
  nl,
  write("             CONTROL"), nl,
  write("         \u250F"), write("\u2501"), repeat(" ", 11), write("\u2501"), write("\u2513"), nl,
  write("         \u2503      1      \u2503"), nl,
  write("                \u2191       "), nl,
  write("           4 \u2190     \u2192 2  "), nl,
  write("                \u2193       "), nl,
  write("         \u2503      3      \u2503"), nl,
  write("         \u2517"), write("\u2501"), repeat(" ", 11), write("\u2501"), write("\u251B"), nl,
  display(B), nl.

play(B) :-
  lose(B),
  write("You lose!"), nl.
play(B1) :-
  show_state(B1),
  read(D), nl,
  slide(B1, D, B2),
  new_number(B2, B3),
  play(B3).

% lose(B) is true if no move is possible that will free up a square in B
%
lose(B) :-
  slide_right(B,B),
  slide_left(B,B),
  slide_up(B,B),
  slide_down(B,B).

% slide(B1, N, B2) is true if B2 is the result of sliding game board B1 in one
% of the four cardinal directions specified by integer N, defined as follows:
%  up:    1
%  right: 2
%  down:  3
%  left:  4
%
% TODO: add recovery
%
slide(B1, 1, B2) :-
  dif(B1, B2),
  slide_up(B1, B2).
slide(B1, 2, B2) :-
  dif(B1, B2),
  slide_right(B1, B2).
slide(B1, 3, B2) :-
  dif(B1, B2),
  slide_down(B1, B2).
slide(B1, 4, B2) :-
  dif(B1, B2),
  slide_left(B1, B2).
slide(B, _, B) :- play(B).

% slide_right(B1, B2) is true if B2 is the result of sliding board (matrix) B1
% to the right, collapsing all elements to the right according to the standard
% 2048 game rules.
%
% slide_left/up/down accomplish the same thing in the other three directions by
% using slide_right after a sequence of matrix flip and transpose operations.
%
slide_right([], []).
slide_right([H|T1], [R|T2]) :-
  collapse_right(H, R),
  slide_right(T1, T2).
slide_left(B1, B4) :-
  flip(B1, B2),
  slide_right(B2, B3),
  flip(B3, B4).
slide_up(B1, B2) :-
  transpose(B1, B1T),
  slide_left(B1T, B2T),
  transpose(B2T, B2).
slide_down(B1, B2) :-
  transpose(B1, B1T),
  slide_right(B1T, B2T),
  transpose(B2T, B2).

% collapse_right(L1, L2) is true if L2 is the result of moving all nonzero
% elements of list L1 as far as possible to the right, combining pairs of equal
% elements into their sum.
%
% sink_right(E, L1, L2) is true if L2 is the result of sinking nonzero element E
% into list L1 as far right as possible, until it reaches a nonzero element,
% combining with it if they are equal.
%
collapse_right([], []).
collapse_right([0|T1], [0|T2]) :-
  collapse_right(T1, T2).
collapse_right([H|T1], T3) :-
  dif(H, 0),
  collapse_right(T1, T2),
  sink_right(H, T2, T3).
sink_right(E, [], [E]).
sink_right(E, [H|T], [E,H|T]) :-
  dif(E, H),
  dif(H, 0).
sink_right(E, [E|T], [0,EE|T]) :-
  EE is E+E.
sink_right(E, [0|T], [0|R]) :-
  sink_right(E, T, R).

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
size([], 0).
size([_|T], N) :-
  size(T, N0),
  N is N0+1.

% nth(L, N, E) is true if E is the Nth element of list L (1-indexed).
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
repeat(_, 0).
repeat(S, N1) :-
  write(S),
  repeat(S, N0),
  N1 is N0+1.

