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
            {Hints, Puzzle} = sudoku_grid:random_puzzle(fun random_generator/1),
            ?assert(Hints < 54),
            ?assertEqual(81, maps:size(Puzzle)),
            ?assertEqual(Hints, count_hints(Puzzle)),
            ok
        end)
    }.

create_proper_puzzle_test_() ->
    {timeout,
        10,
        ?_test(begin
            Puzzle = sudoku_grid:parallel_random_puzzle(fun random_generator/1, 25, 5000),
            ?assertEqual(81, maps:size(Puzzle)),
            ?assert(count_hints(Puzzle) =< 25),
            ok
        end)
    }.

assert_solution_is_valid(Solution) ->
    ?assertEqual(81, maps:size(Solution)),
    [?assertEqual(1, length(maps:get({X, Y}, Solution))) || X <- lists:seq(1, 9), Y <- lists:seq(1, 9)],
    [?assertEqual(lists:seq(1, 9), lists:sort([hd(maps:get({X, Y}, Solution)) || X <- lists:seq(1, 9)])) || Y <- lists:seq(1, 9)],
    [?assertEqual(lists:seq(1, 9), lists:sort([hd(maps:get({X, Y}, Solution)) || Y <- lists:seq(1, 9)])) || X <- lists:seq(1, 9)],
    [?assertEqual(lists:seq(1, 9), lists:sort([hd(maps:get({X, Y}, Solution)) || X <- lists:seq(SX * 3 + 1, SX * 3 + 3), Y <- lists:seq(SY * 3 + 1, SY * 3 + 3)])) || SY <- lists:seq(0, 2), SX <- lists:seq(0, 2)],
    ok.

count_hints(Puzzle) ->
    maps:fold(fun({_X, _Y}, V, Acc) ->
        case V of
            0 -> Acc;
            _ -> Acc + 1
        end
    end, 0, Puzzle).
