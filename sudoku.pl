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
  valid_rows(ROWS).

valid_rows([]).
valid_rows([ROW | ROWS]) :-
  valid_row(ROW),
  valid_rows(ROWS).

valid_row(ROW) :- length(ROW, 9), are_different_digits(ROW).

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

sudoku(ROWS) :- valid_grid(ROWS).

% [[1, 2, 3, 4, 5, 6, 7, 8, 9], [1, 2, 3, 4, 5, 6, 7, 8, 9], [1, 2, 3, 4, 5, 6, 7, 8, 9], [1, 2, 3, 4, 5, 6, 7, 8, 9], [1, 2, 3, 4, 5, 6, 7, 8, 9], [1, 2, 3, 4, 5, 6, 7, 8, 9], [1, 2, 3, 4, 5, 6, 7, 8, 9], [1, 2, 3, 4, 5, 6, 7, 8, 9], [1, 2, 3, 4, 5, 6, 7, 8, 9]]