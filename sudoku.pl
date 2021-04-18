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

solve(GRID) :-
  length(GRID, 81),
  solve_grid(0, GRID),
  print_solution(GRID).

print_solution(GRID) :-
  format('Solution: ~n'),
  print_grid(GRID).

print_grid([E | GRID]) :-
  format(E), (end_of_row(GRID) -> format('~n'); format(' ')),
  print_grid(GRID).

print_grid([]).

end_of_row(GRID) :-
  length(GRID, SIZE),
  0 is mod(SIZE, 9).

solve_grid(I, GRID) :-
  element(I, GRID, ELEMENT),
  (var(ELEMENT) -> solve_element(I, GRID) ; true),
  NEXT_I is I + 1,
  solve_grid(NEXT_I, GRID).

solve_grid(81, _).

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
  not(memberchk(ELEMENT, SET)).

solve_element([HR | ROW], [HC | COL], [HS | SQUARE], SET, ELEMENT) :-
  (nonvar(HR), not(memberchk(HR, SET)) -> append(SET, [HR], SET_WITH_ROW) ; SET_WITH_ROW = SET),
  (nonvar(HC), not(memberchk(HC, SET_WITH_ROW)) -> append(SET_WITH_ROW, [HC], SET_WITH_COL) ; SET_WITH_COL = SET_WITH_ROW),
  (nonvar(HS), not(memberchk(HS, SET_WITH_COL)) -> append(SET_WITH_COL, [HS], FINAL_SET) ; FINAL_SET = SET_WITH_COL),
  solve_element(ROW, COL, SQUARE, FINAL_SET, ELEMENT).

get_row_col_square(ELEMENT_INDEX, GRID, RESULT_ROW, RESULT_COL, RESULT_SQUARE) :-
  get_row_col_square(ELEMENT_INDEX, 0, GRID, [], [], [], RESULT_ROW, RESULT_COL, RESULT_SQUARE).

get_row_col_square(_, 81, _, ROW, COL, SQUARE, ROW, COL, SQUARE).

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

%First example

% solve([A, B, 4, 8, C, D, E, 1, 7, 6, 7, F, 9, G, H, I, J, K, 5, L, 8, M, 3, N, O, P, 4, 3, Q, R, 7, 4, S, 1, T, U, V, 6, 9, W, X, Y, 7, 8, Z, AA, AB, 1, AC, 6, 9, AD, AE, 5, 1, AF, AG, AH, 8, AI, 3, AJ, 6, AK, AL, AM, AN, AO, 6, AP, 9, 1, 2, 4, AQ, AR, AS, 1, 5, AT, AU]).

% A , B , 4 , 8 , C , D , E , 1 , 7 ,
% 6 , 7 , F , 9 , G , H , I , J , K ,
% 5 , L , 8 , M , 3 , N , O , P , 4 ,
% 3 , Q , R , 7 , 4 , S , 1 , T , U ,
% V , 6 , 9 , W , X , Y , 7 , 8 , Z ,
% AA, AB, 1 , AC, 6 , 9 , AD, AE, 5 ,
% 1 , AF, AG, AH, 8 , AI, 3 , AJ, 6 ,
% AK, AL, AM, AN, AO, 6 , AP, 9 , 1 ,
% 2 , 4 , AQ, AR, AS, 1 , 5 , AT, AU

% Solution

% [9,3,4,8,2,5,6,1,7,
%  6,7,2,9,1,4,8,5,3,
%  5,1,8,6,3,7,9,2,4,
%  3,2,5,7,4,8,1,6,9,
%  4,6,9,1,5,3,7,8,2,
%  7,8,1,2,6,9,4,3,5,
%  1,9,7,5,8,2,3,4,6,
%  8,5,3,4,7,6,2,9,1,
%  2,4,6,3,9,1,5,7,8]

%Second example

% solve([3, A, B, C, 7, D, E, F, 2, G, H, I, 6, J, 8, K, L, M, N, O, 9, P, 2, Q, 1, R, S, T, 1, U, V, W, X, Y, 3, Z, 4, AA, 3, AB, 5, AC, 6, AD, 8, AE, 5, AF, AG, AH, AI, AJ, 4, AK, AL, AM, 8, AN, 6, AO, 9, AP, AQ, AR, AS, AT, 5, AU, 9, AV, AW, AX, 7, AY, AZ, BA, 8, BB, BC, BD, 4]).

% 3 , A , B , C , 7 , D , E , F , 2 ,
% G , H , I , 6 , J , 8 , K , L , M ,
% N , O , 9,  P , 2 , Q , 1 , R , S ,
% T , 1 , U , V , W , X , Y , 3 , Z ,
% 4 , AA, 3 , AB, 5 , AC, 6 , AD, 8 ,
% AE, 5 , AF, AG, AH, AI, AJ, 4 , AK,
% AL, AM, 8 , AN, 6 , AO, 9 , AP, AQ,
% AR, AS, AT, 5 , AU, 9 , AV, AW, AX,
% 7 , AY, AZ, BA, 8 , BB, BC, BD, 4

% Solution

% [3,6,4,1,7,5,8,9,2,
%  2,7,1,6,9,8,4,5,3,
%  5,8,9,4,2,3,1,7,6,
%  9,1,6,8,4,2,5,3,7,
%  4,2,3,9,5,7,6,1,8,
%  8,5,7,3,1,6,2,4,9,
%  1,3,8,7,6,4,9,2,5,
%  6,4,2,5,3,9,7,8,1,
%  7,9,5,2,8,1,3,6,4]

% Example with multiple solutions

% [2, 9, 5, 7, 4, 3, 8, 6, 1, 4, 3, 1, 8, 6, 5, 9, A, B, 8, 7, 6, 1, 9, 2, 5, 4, 3, 3, 8, 7, 4, 5, 9, 2, 1, 6, 6, 1, 2, 3, 8, 7, 4, 9, 5, 5, 4, 9, 2, 1, 6, 7, 3, 8, 7, 6, 3, 5, 3, 4, 1, 8, 9, 9, 2, 8, 6, 7, 1, 3, 5, 4, 1, 5, 4, 9, 3, 8, 6, C, D]

% [2, 9, 5, 7, 4, 3, 8, 6, 1,
%  4, 3, 1, 8, 6, 5, 9, A, B,
%  8, 7, 6, 1, 9, 2, 5, 4, 3,
%  3, 8, 7, 4, 5, 9, 2, 1, 6,
%  6, 1, 2, 3, 8, 7, 4, 9, 5,
%  5, 4, 9, 2, 1, 6, 7, 3, 8,
%  7, 6, 3, 5, 3, 4, 1, 8, 9,
%  9, 2, 8, 6, 7, 1, 3, 5, 4,
%  1, 5, 4, 9, 3, 8, 6, C, D]