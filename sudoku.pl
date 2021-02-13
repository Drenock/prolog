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

%  --------------------------------------------------------------------------

valid_grid(ROWS) :-
  length(ROWS, 9),
  valid_rows(ROWS).

valid_rows([]).
valid_rows([ROW | ROWS]) :-
  valid_row(ROW),
  valid_rows(ROWS).

valid_row(ROW) :- length(ROW, 9), are_different_digits(ROW).

get_col([], TEMP, TEMP).

get_col([[ELEMENT | _] | ROWS], [], COL) :-
  append([ELEMENT], [], RESULT),
  get_col(ROWS, RESULT, COL).

get_col([[ELEMENT | _] | ROWS], TEMP, COL) :-
  not(length(TEMP, 0)),
  append([ELEMENT], TEMP, RESULT),
  get_col(ROWS, RESULT, COL).

sudoku(ROWS) :- valid_grid(ROWS).

% [[1, 2, 3, 4, 5, 6, 7, 8, 9], [1, 2, 3, 4, 5, 6, 7, 8, 9], [1, 2, 3, 4, 5, 6, 7, 8, 9], [1, 2, 3, 4, 5, 6, 7, 8, 9], [1, 2, 3, 4, 5, 6, 7, 8, 9], [1, 2, 3, 4, 5, 6, 7, 8, 9], [1, 2, 3, 4, 5, 6, 7, 8, 9], [1, 2, 3, 4, 5, 6, 7, 8, 9], [1, 2, 3, 4, 5, 6, 7, 8, 9]]