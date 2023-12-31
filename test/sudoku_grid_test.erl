% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(sudoku_grid_test).
-include_lib("eunit/include/eunit.hrl").

random_generator(N) ->
    case get(rand_seed) of
        undefined -> rand:seed(exs1024s, {123, 123534, 345345});
        _ -> ok
    end,
    rand:uniform(N).

create_solution_test_() ->
    {timeout,
        120,
        ?_test(begin
            Solution = sudoku_grid:random_solution(fun random_generator/1),
            assert_solution_is_valid(Solution)
        end)
    }.

create_puzzle_test_() ->
    {timeout,
        120,
        ?_test(begin
            {Hints, Puzzle, Solution} = sudoku_grid:random_puzzle(fun random_generator/1),
            ?assert(Hints < 54),
            ?assertEqual(81, length(sudoku_grid:to_list(Puzzle))),
            ?assertEqual(Hints, count_hints(Puzzle)),
            ?assertEqual(81, length(sudoku_grid:to_list(Solution))),
            ?assertEqual(81, count_hints(Solution)),
            sudoku_grid:is_solved(Solution),
            ok
        end)
    }.

create_proper_puzzle_test_() ->
    {timeout,
        10,
        ?_test(begin
            {Puzzle, Solution} = sudoku_grid:parallel_random_puzzle(fun random_generator/1, 25, 5000),
            ?assertEqual(81, length(sudoku_grid:to_list(Puzzle))),
            ?assert(count_hints(Puzzle) =< 25),
            ?assertEqual(81, length(sudoku_grid:to_list(Solution))),
            ?assertEqual(81, count_hints(Solution)),
            sudoku_grid:is_solved(Solution),
            ok
        end)
    }.

assert_solution_is_valid(Solution) ->
    ?assertEqual(81, length(sudoku_grid:to_list(Solution))),
    [?assertEqual(1, length(sudoku_grid:get(X, Y, Solution))) || X <- lists:seq(1, 9), Y <- lists:seq(1, 9)],
    [?assertEqual(lists:seq(1, 9), lists:sort([hd(sudoku_grid:get(X, Y, Solution)) || X <- lists:seq(1, 9)])) || Y <- lists:seq(1, 9)],
    [?assertEqual(lists:seq(1, 9), lists:sort([hd(sudoku_grid:get(X, Y, Solution)) || Y <- lists:seq(1, 9)])) || X <- lists:seq(1, 9)],
    [?assertEqual(lists:seq(1, 9), lists:sort([hd(sudoku_grid:get(X, Y, Solution)) || X <- lists:seq(SX * 3 + 1, SX * 3 + 3), Y <- lists:seq(SY * 3 + 1, SY * 3 + 3)])) || SY <- lists:seq(0, 2), SX <- lists:seq(0, 2)],
    ok.

count_hints(Puzzle) ->
    lists:foldl(fun({{_X, _Y}, V}, Acc) ->
        case V of
            0 -> Acc;
            _ -> Acc + 1
        end
    end, 0, sudoku_grid:to_list(Puzzle)).

is_move_valid_test() ->
    EmptyGrid = maps:from_list([{{X, Y}, 0} || X <- lists:seq(1, 9), Y <- lists:seq(1, 9)]),
    ?assert(sudoku_grid:is_move_valid(EmptyGrid, {1, 1}, 1)),
    Grid1 = maps:put({1, 1}, 1, EmptyGrid),
    ?assertNot(sudoku_grid:is_move_valid(Grid1, {1, 9}, 1)),
    ?assertNot(sudoku_grid:is_move_valid(Grid1, {9, 1}, 1)),
    ?assertNot(sudoku_grid:is_move_valid(Grid1, {2, 2}, 1)),
    ok.

is_solved_test() ->
    EmptyGrid = maps:from_list([{{X, Y}, 0} || X <- lists:seq(1, 9), Y <- lists:seq(1, 9)]),
    ?assertNot(sudoku_grid:is_solved(EmptyGrid)),
    FullYetInvalidGrid = maps:from_list([{{X, Y}, X} || X <- lists:seq(1, 9), Y <- lists:seq(1, 9)]),
    ?assert(sudoku_grid:is_solved(FullYetInvalidGrid)),
    ok.
