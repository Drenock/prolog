 is_digit(D) :-
   between(1, 9, D).

is_array_of_digits([]).
is_array_of_digits([H | T]) :-
  is_digit(H),
  is_array_of_digits(T).

is_two_dimensional_array_of_digits([]).
is_two_dimensional_array_of_digits([H | T]) :-
  is_array_of_digits(H),
  is_two_dimensional_array_of_digits(T).

are_different([]).

are_different([H1 | T1]) :-
  not(memberchk(H1, T1)),
  are_different(T1).

all_empty([]).
all_empty([ROW | ROWS]) :-
  length(ROW, 0),
  all_empty(ROWS).

%  --------------------------------------------------------------------------

valid_grid(ROWS) :-
  is_two_dimensional_array_of_digits(ROWS),
  length(ROWS, 9),
  get_cols(ROWS, COLS),
  get_squares(ROWS, SQUARES),
  valid_sets(ROWS),
  valid_sets(COLS),
  valid_sets(SQUARES).

valid_sets([]).
valid_sets([ROW | ROWS]) :-
  valid_set(ROW),
  valid_sets(ROWS).

valid_set(ROW) :- length(ROW, 9), are_different(ROW).

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

% -------------------------------------------------------

solve(GRID) :-
  length(GRID, 81),
  solve_grid(0, GRID),
  print(GRID).

solve_grid(I, GRID) :-
  element(I, GRID, ELEMENT),
  (var(ELEMENT) -> print("Solving "), print(I), format("~n"), solve_element(I, GRID) ; true),
  NEXT_I is I + 1,
  solve_grid(NEXT_I, GRID).

solve_grid(80, _).

for(_, STOP, STOP, _) :- print('For loop ended').

element(INDEX, [_ | T], ELEMENT) :-
  NEW_INDEX is INDEX - 1,
  element(NEW_INDEX, T, ELEMENT).

element(0, [ELEMENT | _], ELEMENT).

solve_element(ELEMENT_INDEX, GRID) :-
  get_row_col_square(ELEMENT_INDEX, GRID, ROW, COL, SQUARE),
  element(ELEMENT_INDEX, GRID, ELEMENT),
  solve_element(ROW, COL, SQUARE, [], ELEMENT).

solve_element([], [], [], SET, ELEMENT) :-
  is_digit(ELEMENT),
  print("guess: "),
  print(ELEMENT),
  format("~n"),
  not(memberchk(ELEMENT, SET)).

solve_element([HR | ROW], [HC | COL], [HS | SQUARE], SET, ELEMENT) :-
  (nonvar(HR), not(memberchk(HR, SET)) -> append(SET, [HR], SET_WITH_ROW) ; SET_WITH_ROW = SET),
  (nonvar(HC), not(memberchk(HC, SET_WITH_ROW)) -> append(SET_WITH_ROW, [HC], SET_WITH_COL) ; SET_WITH_COL = SET_WITH_ROW),
  (nonvar(HS), not(memberchk(HS, SET_WITH_COL)) -> append(SET_WITH_COL, [HS], FINAL_SET) ; FINAL_SET = SET_WITH_COL),
  solve_element(ROW, COL, SQUARE, FINAL_SET, ELEMENT).

get_row_col_square(ELEMENT_INDEX, GRID, RESULT_ROW, RESULT_COL, RESULT_SQUARE) :-
  get_row_col_square(ELEMENT_INDEX, 0, GRID, [], [], [], RESULT_ROW, RESULT_COL, RESULT_SQUARE).

get_row_col_square(_, 80, _, ROW, COL, SQUARE, ROW, COL, SQUARE).

get_row_col_square(ELEMENT_INDEX, I, GRID, ROW, COL, SQUARE, RESULT_ROW, RESULT_COL, RESULT_SQUARE) :-
  element(I, GRID, I_ELEMENT),
  get_indexes(ELEMENT_INDEX, ROW_INDEX, COL_INDEX, SQUARE_INDEX),
  get_indexes(I, I_ROW_INDEX, I_COL_INDEX, I_SQUARE_INDEX),
  (I_ROW_INDEX = ROW_INDEX -> append(ROW, [I_ELEMENT], NEW_ROW) ; NEW_ROW = ROW),
  (I_COL_INDEX = COL_INDEX -> append(COL, [I_ELEMENT], NEW_COL) ; NEW_COL = COL),
  (I_SQUARE_INDEX = SQUARE_INDEX -> append(SQUARE, [I_ELEMENT], NEW_SQUARE) ; NEW_SQUARE = SQUARE),
  NEXT_I is I + 1,
  get_row_col_square(ELEMENT_INDEX, NEXT_I, GRID, NEW_ROW, NEW_COL, NEW_SQUARE, RESULT_ROW, RESULT_COL, RESULT_SQUARE).

get_indexes(ELEMENT_INDEX, ROW_INDEX, COL_INDEX, SQUARE_INDEX) :-
  get_row_index(ELEMENT_INDEX, ROW_INDEX),
  get_col_index(ELEMENT_INDEX, COL_INDEX),
  get_square_index(ROW_INDEX, COL_INDEX, SQUARE_INDEX).

get_row_index(ELEMENT_INDEX, ROW_INDEX) :-
  ROW_INDEX is div(ELEMENT_INDEX, 9).

get_col_index(ELEMENT_INDEX, COL_INDEX) :-
  COL_INDEX is mod(ELEMENT_INDEX, 9).

get_square_index(ROW_INDEX, COL_INDEX, SQUARE_INDEX) :-
  SQUARE_INDEX is div(ROW_INDEX, 3) * 3 + div(COL_INDEX, 3).

%  (solved(GRID) -> print(GRID) ; solve_grid(GRID)).

% [1, 2, 3, 4, 5, 6, 7, 8, 9, 4, 5, 6, 7, 8, 9, 1, 2, 3, 7, 8, 9, 1, 2, 3, 4, 5, 6, 5, 6, 7, 8, 9, 1, 2, 3, 4, 8, 9, 1, 2, 3, 4, 5, 6, 7, 2, 3, 4, 5, 6, 7, 8, 9, 1, 6, 7, 8, 9, 1, 2, 3, 4, 5, 9, 1, 2, 3, 4, 5, 6, 7, 8, 3, 4, 5, 6, 7, 8, 9, 1, 2]
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

% [A, B, 3, 4, 5, 6, 7, 8, 9]
% [4, 5, 6, 7, 8, 9, 1, 2, 3]
% [7, 8, 9, 1, 2, 3, 4, 5, 6]
% [5, 6, 7, 8, 9, 1, 2, 3, 4]
% [8, 9, 1, 2, 3, 4, 5, 6, 7]
% [C, 3, 4, 5, 6, 7, 8, 9, 1]
% [6, 7, 8, 9, 1, 2, 3, 4, 5]
% [9, 1, 2, 3, 4, 5, 6, 7, 8]
% [3, 4, 5, 6, 7, 8, 9, 1, 2]

% [A, B, 3, 4, 5, 6, 7, 8, 9, 4, 5, 6, 7, 8, 9, 1, 2, 3, 7, 8, 9, 1, 2, 3, 4, 5, 6, 5, 6, 7, 8, 9, 1, 2, 3, 4, 8, 9, 1, 2, 3, 4, 5, 6, 7, C, 3, 4, 5, 6, 7, 8, 9, 1, 6, 7, 8, 9, 1, 2, 3, 4, 5, 9, 1, 2, 3, 4, 5, 6, 7, 8, 3, 4, 5, 6, 7, 8, 9, 1, 2]

% [1, 2, 3, 4, 5, 6, 7, 8, 9, 4, 5, 6, 7, 8, 9, 1, 2, 3, 7, 8, 9, 1, 2, 3, 4, 5, 6, 5, 6, 7, 8, 9, 1, 2, 3, 4, 8, 9, 1, 2, 3, 4, 5, 6, 7, 2, 3, 4, 5, 6, 7, 8, 9, 1, 6, 7, 8, 9, 1, 2, 3, 4, 5, 9, 1, 2, 3, 4, 5, 6, 7, 8, 3, 4, 5, 6, 7, 8, 9, 1, 2]

% [A, B, 4, 8, C, D, E, 1, 7, 6, 7, F, 9, G, H, I, J, K, 5, L, 8, M, 3, N, O, P, 4, 3, Q, R, 7, 4, S, 1, T, U, V, 6, 9, W, X, Y, 7, 8, Z, AA, AB, 1, AC, 6, 9, AD, AE, 5, 1, AF, AG, AH, 8, AI, 3, AJ, 6, AK, AL, AM, AN, AO, 6, AP, 9, 1, 2, 4, AQ, AR, AS, 1, 5, AT, AU]

% A , B , 4 , 8 , C , D , E , 1 , 7 ,
% 6 , 7 , F , 9 , G , H , I , J , K ,
% 5 , L , 8 , M , 3 , N , O , P , 4 ,
% 3 , Q , R , 7 , 4 , S , 1 , T , U ,
% V , 6 , 9 , W , X , Y , 7 , 8 , Z ,
% AA, AB, 1 , AC, 6 , 9 , AD, AE, 5 ,
% 1 , AF, AG, AH, 8 , AI, 3 , AJ, 6 ,
% AK, AL, AM, AN, AO, 6 , AP, 9 , 1 ,
% 2 , 4 , AQ, AR, AS, 1 , 5 , AT, AU