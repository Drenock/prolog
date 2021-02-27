is_digit(D) :-
  between(1, 9, D).

are_digits([]).

are_digits([H | T]) :-
  is_digit(H),
  are_digits(T).

are_different([]).

are_different([H1 | T1]) :-
  not(memberchk(H1, T1)),
  are_different(T1).

are_different_digits(A) :-
  are_digits(A),
  are_different(A).

all_empty([]).
all_empty([ROW | ROWS]) :-
  length(ROW, 0),
  all_empty(ROWS).

%  --------------------------------------------------------------------------

valid_grid(ROWS) :-
  length(ROWS, 9),
  valid_sets(ROWS),
  get_cols(ROWS, COLS),
  valid_sets(COLS),
  get_squares(ROWS, SQUARES),
  valid_sets(SQUARES).

valid_sets([]).
valid_sets([ROW | ROWS]) :-
  valid_set(ROW),
  valid_sets(ROWS).

valid_set(ROW) :- length(ROW, 9), are_different_digits(ROW).

% stop
get_cols(EMPTY_ROWS, [], [], RESULT, RESULT) :-
  all_empty(EMPTY_ROWS).

% next col
get_cols([], TRIMMED_ROWS, FINISHED_TEMP_COL, CURRENT_TEMP_COLS, RESULT) :-
  append([FINISHED_TEMP_COL], CURRENT_TEMP_COLS, NEXT_TEMP_COLS),
  get_cols(TRIMMED_ROWS, [], [], NEXT_TEMP_COLS, RESULT).

% process col
get_cols([[ELEMENT | ROW] | ROWS], CURRENT_TRIMMED_ROWS, CURRENT_TEMP_COL, TEMP_COLS, RESULT) :-
  append([ELEMENT], CURRENT_TEMP_COL, NEXT_TEMP_COL),
  append([ROW], CURRENT_TRIMMED_ROWS, NEXT_TRIMMED_ROWS),
  get_cols(ROWS, NEXT_TRIMMED_ROWS, NEXT_TEMP_COL, TEMP_COLS, RESULT).

get_cols(ROWS, RESULT) :- get_cols(ROWS, [], [], [], RESULT).

% stop
get_squares(EMPTY_ROWS, [], [], RESULT, RESULT) :-
  all_empty(EMPTY_ROWS),
  length(RESULT, 9).

% next col
get_squares([], TRIMMED_ROWS, FINISHED_TEMP_SQUARE, CURRENT_TEMP_SQUARES, RESULT) :-
  append([FINISHED_TEMP_SQUARE], CURRENT_TEMP_SQUARES, NEXT_TEMP_SQUARES),
  get_squares(TRIMMED_ROWS, [], [], NEXT_TEMP_SQUARES, RESULT).

get_squares(ROWS, TRIMMED_ROWS, FINISHED_TEMP_SQUARE, CURRENT_TEMP_SQUARES, RESULT) :-
  not(length(ROWS, 0)),
  length(FINISHED_TEMP_SQUARE, 9),
  append([FINISHED_TEMP_SQUARE], CURRENT_TEMP_SQUARES, NEXT_TEMP_SQUARES),
  get_squares(ROWS, TRIMMED_ROWS, [], NEXT_TEMP_SQUARES, RESULT).

% process col
get_squares([[E1, E2, E3 | ROW] | ROWS], CURRENT_TRIMMED_ROWS, CURRENT_TEMP_SQUARE, TEMP_SQUARES, RESULT) :-
  append([E1, E2, E3], CURRENT_TEMP_SQUARE, NEXT_TEMP_SQUARE),
  append([ROW], CURRENT_TRIMMED_ROWS, NEXT_TRIMMED_ROWS),
  get_squares(ROWS, NEXT_TRIMMED_ROWS, NEXT_TEMP_SQUARE, TEMP_SQUARES, RESULT).

get_squares(ROWS, RESULT) :- get_squares(ROWS, [], [], [], RESULT), !.

sudoku(ROWS) :- valid_grid(ROWS).

% [[1, 2, 3, 4, 5, 6, 7, 8, 9], [4, 5, 6, 7, 8, 9, 1, 2, 3], [7, 8, 9, 1, 2, 3, 4, 5, 6], [5, 6, 7, 8, 9, 1, 2, 3, 4], [8, 9, 1, 2, 3, 4, 5, 6, 7], [2, 3, 4, 5, 6, 7, 8, 9, 1], [6, 7, 8, 9, 1, 2, 3, 4, 5], [9, 1, 2, 3, 4, 5, 6, 7, 8], [3, 4, 5, 6, 7, 8, 9, 1, 2]]

% [1, 2, 3, 4, 5, 6, 7, 8, 9]
% [2, 3, 4, 5, 6, 7, 8, 9, 1]
% [3, 4, 5, 6, 7, 8, 9, 1, 2]
% [4, 5, 6, 7, 8, 9, 1, 2, 3]
% [5, 6, 7, 8, 9, 1, 2, 3, 4]
% [6, 7, 8, 9, 1, 2, 3, 4, 5]
% [7, 8, 9, 1, 2, 3, 4, 5, 6]
% [8, 9, 1, 2, 3, 4, 5, 6, 7]
% [9, 1, 2, 3, 4, 5, 6, 7, 8]

% [1, 2, 3, 4, 5, 6, 7, 8, 9]
% [4, 5, 6, 7, 8, 9, 1, 2, 3]
% [7, 8, 9, 1, 2, 3, 4, 5, 6]
% [5, 6, 7, 8, 9, 1, 2, 3, 4]
% [8, 9, 1, 2, 3, 4, 5, 6, 7]
% [2, 3, 4, 5, 6, 7, 8, 9, 1]
% [6, 7, 8, 9, 1, 2, 3, 4, 5]
% [9, 1, 2, 3, 4, 5, 6, 7, 8]
% [3, 4, 5, 6, 7, 8, 9, 1, 2]

% [[A, B, 4, 8, C, D, E, 1, 7], [6, 7, F, 9, G, H, I, J, K], [5, L, 8, M, 3, N, O, P, 4], [3, Q, R, 7, 4, S, 1, T, U], [V, 6, 9, W, X, Y, 7, 8, Z], [AA, AB, 1, AC, 6, 9, AD, AE, 5], [1, AF, AG, AH, 8, AI, 3, AJ, 6], [AK, AL, AM, AN, AO, 6, AP, 9, 1], [2, 4, AQ, AR, AS, 1, 5, AT, AU]]