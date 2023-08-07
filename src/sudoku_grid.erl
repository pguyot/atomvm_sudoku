% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(sudoku_grid).

-export([random_puzzle/1, random_solution/1, parallel_random_puzzle/3, print/1]).

-type puzzle_grid() :: #{{1..9, 1..9} => 0..9}.
-type work_grid() :: #{{1..9, 1..9} => [1..9]}.
-type random_generator() :: fun((N :: pos_integer()) -> X :: pos_integer()).

-export_type([puzzle_grid/0]).

-spec parallel_random_puzzle(random_generator(), pos_integer(), timeout()) -> puzzle_grid().
parallel_random_puzzle(RandomGenerator, MaxHint, Timeout) ->
    Parent = self(),
    Start = erlang:system_time(millisecond),
    Workers = [spawn_opt(fun() -> parallel_random_puzzle_worker_loop(RandomGenerator, Parent, infinity) end, [monitor]) || _ <- lists:seq(1, erlang:system_info(schedulers_online))],
    parallel_random_puzzle_loop(Start, MaxHint, Workers, Timeout, infinity, undefined).

parallel_random_puzzle_worker_loop(RandomGenerator, Parent, BestCandidate) ->
    {Hints, Puzzle} = random_puzzle(RandomGenerator),
    if
        Hints < BestCandidate ->
            Parent ! {self(), Hints, Puzzle},
            parallel_random_puzzle_worker_loop(RandomGenerator, Parent, Hints);
        true ->
            parallel_random_puzzle_worker_loop(RandomGenerator, Parent, BestCandidate)
    end.

parallel_random_puzzle_loop(Start, MaxHint, Workers, Timeout, BestPuzzleHints, BestPuzzleGrid) ->
    Wait = case Timeout of
        infinity -> infinity;
        _ -> Timeout + Start - erlang:system_time(millisecond)
    end,
    receive
        {_Worker, Hints, Puzzle} ->
            if
                Hints =< MaxHint ->
                    stop_workers(Workers),
                    Puzzle;
                Hints < BestPuzzleHints ->
                    parallel_random_puzzle_loop(Start, MaxHint, Workers, Timeout, Hints, Puzzle);
                true ->
                    parallel_random_puzzle_loop(Start, MaxHint, Workers, Timeout, BestPuzzleHints, BestPuzzleGrid)
            end
    after Wait ->
        [exit(Worker, kill) || Worker <- Workers],
        BestPuzzleGrid
    end.

stop_workers([]) -> ok;
stop_workers([{Worker, Monitor} | Tail]) ->
    exit(Worker, kill),
    demonitor(Monitor, [flush]),
    flush_solutions(Worker),
    stop_workers(Tail).

flush_solutions(Worker) ->
    receive {Worker, _, _} -> flush_solutions(Worker) after 0 -> ok end.

-spec random_puzzle(random_generator()) -> {pos_integer(), puzzle_grid()}.
random_puzzle(RandomGenerator) ->
    Grid = random_solution(RandomGenerator),
    AllCells = lists:sort(maps:keys(Grid)), % make result deterministic
    ShuffledCells = shuffle(RandomGenerator, AllCells),
    remove_values_until_multiple_solutions(Grid, ShuffledCells, []).

-spec shuffle(random_generator(), [any()]) -> [any()].
shuffle(RandomGenerator, List) ->
    [Val || {_, Val} <- lists:sort([{RandomGenerator(16#10000), Val} || Val <- List])].

remove_values_until_multiple_solutions(Solution, [], HintCells) ->
    {length(HintCells), hints_to_puzzle_grid(Solution, HintCells)};
remove_values_until_multiple_solutions(Solution, [Cell | Tail], AccHintCells) ->
    Candidate = remove_cells_and_propagate(Solution, Tail ++ AccHintCells),
    case has_a_unique_solution(Candidate) of
        true ->
            remove_values_until_multiple_solutions(Solution, Tail, AccHintCells);
        false ->
            remove_values_until_multiple_solutions(Solution, Tail, [Cell | AccHintCells])
    end.

-spec random_solution(random_generator()) -> work_grid().
random_solution(RandomGenerator) ->
    AllValues = lists:seq(1, 9),
    EmptyGrid = maps:from_list([{{X, Y}, AllValues} || X <- lists:seq(1, 9), Y <- lists:seq(1, 9)]),
    fill_random_grid(RandomGenerator, EmptyGrid).

fill_random_grid(RandomGenerator, EmptyGrid) ->
    RandomValues = random_values(RandomGenerator, 17),
    CandidateGrid = fill_cells(RandomGenerator, EmptyGrid, RandomValues),
    case find_a_solution(CandidateGrid) of
        {value, Solution} -> Solution;
        none -> fill_random_grid(RandomGenerator, EmptyGrid)
    end.

random_values(RandomGenerator, Count) ->
    RandomValues = [RandomGenerator(9) || _ <- lists:seq(1, Count)],
    % We need at least 8 different values
    case length(lists:usort(RandomValues)) < 8 of
        true -> random_values(RandomGenerator, Count);
        false -> RandomValues
    end.

fill_cells(_RandomGenerator, Grid, []) -> Grid;
fill_cells(RandomGenerator, Grid, [Value | Tail]) ->
    RandomCellX = RandomGenerator(9),
    RandomCellY = RandomGenerator(9),
    case maps:get({RandomCellX, RandomCellY}, Grid) of
        [SingleValue] when SingleValue =/= Value ->
            fill_cells(RandomGenerator, Grid, [Value | Tail]);
        _ ->
            NewGrid = set_grid_value_and_propagate(Grid, {RandomCellX, RandomCellY}, Value),
            case NewGrid of
                invalid ->
                    fill_cells(RandomGenerator, Grid, [Value | Tail]);
                _ ->
                    fill_cells(RandomGenerator, NewGrid, Tail)
            end
    end.

set_grid_value_and_propagate(Grid0, {X, Y}, Value) ->
    set_grid_value_and_propagate(Grid0, [{{X, Y}, Value}]).

set_grid_value_and_propagate_update_cell(CellX, CellY, CellValues, Value, AccGrid, AccList) ->
    case lists:member(Value, CellValues) of
        true ->
            NewCellValues = lists:delete(Value, CellValues),
            case NewCellValues of
                [] -> invalid;
                [SingleValue] ->
                    {AccGrid, [{{CellX, CellY}, SingleValue} | AccList]};
                _ ->
                    {maps:update({CellX, CellY}, NewCellValues, AccGrid), AccList}
            end;
        false ->
            {AccGrid, AccList}
    end.

set_grid_value_and_propagate(Grid0, [{{X, Y}, Value} | Tail]) ->
    Result = lists:foldl(fun({CellX, CellY}, Acc) ->
        CellValues = maps:get({CellX, CellY}, Grid0),
        case Acc of
            invalid -> invalid;
            {AccGrid, AccList} ->
                if
                    CellX =:= X andalso CellY =:= Y -> {maps:update({X, Y}, [Value], AccGrid), AccList};
                    CellX =:= X ->
                        set_grid_value_and_propagate_update_cell(CellX, CellY, CellValues, Value, AccGrid, AccList);
                    CellY =:= Y ->
                        set_grid_value_and_propagate_update_cell(CellX, CellY, CellValues, Value, AccGrid, AccList);
                    (CellX - 1) div 3 =:= (X - 1) div 3 andalso (CellY - 1) div 3 =:= (Y - 1) div 3 ->
                        set_grid_value_and_propagate_update_cell(CellX, CellY, CellValues, Value, AccGrid, AccList);
                    true ->
                        {AccGrid, AccList}
                end
        end
    end, {Grid0, Tail}, [{IX, IY} || IX <- lists:seq(1, 9), IY <- lists:seq(1, 9)]), % deterministic, as opposed to maps:fold
    case Result of
        invalid -> invalid;
        {NewGrid, []} -> NewGrid;
        {NewGrid, NewList} ->
            set_grid_value_and_propagate(NewGrid, NewList)
    end.

hints_to_puzzle_grid(Grid, HintCells) ->
    EmptyGrid = maps:from_list([{{X, Y}, 0} || X <- lists:seq(1, 9), Y <- lists:seq(1, 9)]),
    lists:foldl(fun(Index, Map) ->
        [Value] = maps:get(Index, Grid),
        maps:put(Index, Value, Map)
    end, EmptyGrid, HintCells).

remove_cells_and_propagate(Grid, HintCells) ->
    AllValues = lists:seq(1, 9),
    EmptyGrid = maps:from_list([{{X, Y}, AllValues} || X <- lists:seq(1, 9), Y <- lists:seq(1, 9)]),
    lists:foldl(fun(Index, Map) ->
        [Value] = maps:get(Index, Grid),
        set_grid_value_and_propagate(Map, Index, Value)
    end, EmptyGrid, HintCells).

find_a_solution(Grid) ->
    find_another_solution(Grid, undefined).

has_a_unique_solution(Grid) ->
    case find_another_solution(Grid, undefined) of
        {value, Solution} ->
            case find_another_solution(Grid, Solution) of
                none -> true;
                {value, _} -> false
            end;
        none -> false
    end.

find_another_solution(Grid, Solution) ->
    case test_grid(Grid) of
        complete when Grid =/= Solution -> {value, Grid};
        complete when Grid =:= Solution -> none;
        {incomplete, {X, Y}, Values} ->
            test_solutions(Grid, {X, Y}, Values, Solution)
    end.

test_solutions(_Grid, {_X, _Y}, [], _Solution) -> none;
test_solutions(Grid, {X, Y}, [Value | Tail], Solution) ->
    Grid1 = set_grid_value_and_propagate(Grid, {X, Y}, Value),
    case Grid1 of
        invalid ->
            test_solutions(Grid, {X, Y}, Tail, Solution);
        _ ->
            case find_another_solution(Grid1, Solution) of
                {value, OtherSolution} -> {value, OtherSolution};
                none -> test_solutions(Grid, {X, Y}, Tail, Solution)
            end
    end.

test_grid(Grid) ->
    lists:foldl(fun({X, Y}, Acc) ->
        case {maps:get({X, Y}, Grid), Acc} of
            {[_V1, _V2 | _] = List, complete} -> {incomplete, {X, Y}, List};
            {[_V1, _V2 | _] = List, {incomplete, {_OtherX, _OtherY}, OtherList}} when length(List) < length(OtherList) -> {incomplete, {X, Y}, List};
            {_, Acc} -> Acc
        end            
    end, complete, [{X, Y} || X <- lists:seq(1, 9), Y <- lists:seq(1, 9)]). % deterministic, as opposed to maps:fold

-spec print(puzzle_grid() | work_grid()) -> ok.
print(Grid) ->
    lists:foreach(fun(X) ->
        lists:foreach(fun(Y) ->
            Val = maps:get({X, Y}, Grid),
            case Val of
                0 -> io:format(" ");
                [SingleVal] when is_integer(SingleVal) -> io:format("~B", [SingleVal]);
                Val when is_integer(Val) -> io:format("~B", [Val]);
                [] -> io:format("X");
                _ when is_list(Val) -> io:format(".")
            end
        end, lists:seq(1, 9)),
        io:format("\n")
    end, lists:seq(1, 9)).
