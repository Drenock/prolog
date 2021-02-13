% NEUF
% DEUX
% ____
% ONZE

is_digit(D) :-
  between(0, 9, D).

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

add_without_carry_no_overflow(A, B, R) :-
  are_digits([A, B, R]),
  R is A + B.

add_without_carry_overflow(A, B, R) :-
  are_digits([A, B, R]),
  R is A + B - 10.

add_without_carry(A, B, R) :-
  add_without_carry_no_overflow(A, B, R);
  add_without_carry_overflow(A, B, R).

add_with_carry_no_overflow(A, B, R) :-
  are_digits([A, B, R]),
  R is A + B + 1.

add_with_carry_overflow(A, B, R) :-
  are_digits([A, B, R]),
  R is A + B + 1 - 10.

add_with_carry(A, B, R) :-
  add_with_carry_no_overflow(A, B, R);
  add_with_carry_overflow(A, B, R).

add_overflow(A, B, R) :-
  add_without_carry_overflow(A, B, R);
  add_with_carry_overflow(A, B, R).

add_no_overflow(A, B, R) :-
  add_without_carry_no_overflow(A, B, R);
  add_with_carry_no_overflow(A, B, R).

add(A, B, R) :-
  add_without_carry(A, B, R);
  add_with_carry(A, B, R).

solve([A, B, C | []]) :-
  add_no_overflow(A, B, C).

solve([A, B, C, D, E, F | T]) :-
  ((add_overflow(A, B, C), add_with_carry(D, E, F));
  (add_no_overflow(A, B, C), add_without_carry(D, E, F))),
  solve([D, E, F | T]).

% NEUF
% DEUX
% ____
% ONZE

test(COUNT) :- aggregate_all(count, play(N, D, O, E, U, Z, F, X), COUNT).

play(N, D, O, E, U, Z, F, X) :-
  are_different_digits([N, D, O, E, U, Z, F, X]),
  N \= 0,
  D \= 0,
  add_without_carry(F, X, E),
  solve([F, X, E, U, U, Z, E, E, N, N, D, O]),
  1000 * N + 100 * E + 10 * U + F + 1000 * D + 100 * E + 10 * U + X =:= 1000 * O + 100 * N + 10 * Z + E,
  format('~D ~D ~D ~D~n~D ~D ~D ~D~n________~n~D ~D ~D ~D~n~n', [N, E, U, F, D, E, U, X, O, N, Z, E]).