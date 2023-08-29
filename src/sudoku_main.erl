% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(sudoku_main).

-export([start_link/0]).

-behavior(gen_server).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2]).

-include("dom.hrl").

start_link() ->
    gen_server:start_link(?MODULE, [], []).

-record(state, {
    countdown :: integer(),
    start :: integer(),
    grid = undefined :: sudoku_grid:puzzle() | undefined,
    solution = undefined :: sudoku_grid:puzzle() | undefined,
    help_uses = 0 :: non_neg_integer()
}).

-define(COUNT_DOWN, 10).
-define(MAX_PROCESSING_TIME, ?COUNT_DOWN * 1000 - 500).

init([]) ->
    LoaderScript = loader_init_script(?COUNT_DOWN),
    emscripten:run_script(LoaderScript, [main_thread, async]),
    Self = self(),
    Start = erlang:system_time(millisecond),
    spawn_link(fun() -> generate_grid(Self) end),
    {ok, #state{countdown = ?COUNT_DOWN, start = Start}, 1000}.

handle_cast(_Msg, #state{} = State0) ->
    {noreply, State0}.

handle_call(_Request, _From, State) ->
    {reply, {error, unimplemented}, State}.

handle_info(timeout, State) ->
    UpdateScript = loader_update_script(State#state.countdown),
    emscripten:run_script(UpdateScript, [main_thread, async]),
    case State#state.countdown of
        0 -> {noreply, State};
        N -> {noreply, State#state{countdown = N - 1}, 1000}
    end;
handle_info({puzzle, Puzzle, Solution}, State) ->
    End = erlang:system_time(millisecond),
    Delta = End - State#state.start,
    PuzzleScript = create_puzzle_script(Puzzle, Delta),
    emscripten:run_script(PuzzleScript, [main_thread, async]),
    maps:foreach(
        fun({X, Y}, Val) ->
            case Val of
                0 ->
                    _ = emscripten:register_click_callback(["#", puzzle_cell_id(X, Y)], [], {X, Y}),
                    ok;
                _ ->
                    ok
            end
        end,
        Puzzle
    ),
    emscripten:register_click_callback(<<"button.help">>, [], help),
    {noreply, State#state{grid = Puzzle, solution = Solution, start = End}};
handle_info(
    {emscripten, {click, _ClickEvent}, help},
    #state{grid = Grid, solution = Solution, help_uses = HelpUses} = State
) ->
    {UpdateScript, NewGrid} = help_script(Grid, Solution),
    emscripten:run_script(UpdateScript, [main_thread, async]),
    {noreply, State#state{grid = NewGrid, help_uses = HelpUses + 1}};
handle_info({emscripten, {click, _ClickEvent}, {_X, _Y} = Position}, #state{grid = Grid} = State) ->
    End = erlang:system_time(millisecond),
    OldValue = maps:get(Position, Grid),
    NewValue = find_next_value(Grid, Position, OldValue),
    NewGrid = maps:put(Position, NewValue, Grid),
    UpdateScript = puzzle_update_value_script(Position, NewValue),
    emscripten:run_script(UpdateScript, [main_thread, async]),
    case sudoku_grid:is_solved(NewGrid) of
        true ->
            SolvedScript = puzzle_solved_script(End - State#state.start, State#state.help_uses),
            emscripten:run_script(SolvedScript, [main_thread, async]);
        false ->
            ok
    end,
    {noreply, State#state{grid = NewGrid}}.

terminate(_Reason, #state{}) ->
    ok.

loader_init_script(CountDown) ->
    {Script, _} = dom:append_child_script(<<"#root">>, loader_element(CountDown), 1),
    Script.

loader_update_script(N) when N > 0 ->
    {Script, _} = dom:replace_children_script(<<".loader-countdown">>, integer_to_binary(N), 1),
    Script;
loader_update_script(0) ->
    {Script, _} = dom:replace_children_script(<<"#loader">>, late_loader_element(), 1),
    Script.

create_puzzle_script(Puzzle, Delta) ->
    {Script, _} = dom:replace_with_script(<<"#loader">>, puzzle_table(Puzzle, Delta), 1),
    Script.

loader_element(N) ->
    #element{
        name = 'div',
        attributes = [{id, <<"loader">>}],
        children = [
            #element{
                name = 'div',
                attributes = [{class, <<"loader-countdown">>}],
                children = [integer_to_binary(N)]
            },
            #element{
                name = p,
                attributes = [{class, <<"loader-caption">>}],
                children = [<<"Creating a completely random new grid">>]
            }
        ]
    }.

late_loader_element() ->
    #element{
        name = 'div',
        attributes = [{class, <<"late-loader-caption">>}],
        children = [<<"Sorry, AtomVM is still creating a grid...">>]
    }.

puzzle_table(Puzzle, Delta) ->
    #element{
        name = 'table',
        attributes = [{class, <<"sudoku-grid">>}],
        children =
            [
                puzzle_caption(Delta),
                puzzle_help_header(),
                puzzle_tbody(Puzzle)
            ]
    }.

puzzle_tbody(Puzzle) ->
    #element{
        name = 'tbody',
        children = [puzzle_row(X, Puzzle) || X <- lists:seq(1, 9)]
    }.

puzzle_caption(Delta) ->
    #element{
        name = 'caption',
        children = [list_to_binary(io_lib:format("Grid generated by AtomVM in ~Bms", [Delta]))]
    }.

puzzle_help_header() ->
    #element{
        name = 'thead',
        children =
            [
                #element{
                    name = 'tr',
                    attributes = [{colspan, <<"9">>}],
                    children = [
                        #element{
                            name = 'th',
                            attributes = [{colspan, <<"9">>}],
                            children = [help_button()]
                        }
                    ]
                }
            ]
    }.

help_button() ->
    #element{
        name = 'button',
        attributes = [{class, <<"help">>}],
        children = [<<"help">>]
    }.

puzzle_row(X, Puzzle) ->
    #element{name = 'tr', children = [puzzle_cell(X, Y, Puzzle) || Y <- lists:seq(1, 9)]}.

-spec puzzle_cell_id(1..9, 1..9) -> binary().
puzzle_cell_id(X, Y) ->
    iolist_to_binary(io_lib:format("cell-~B-~B", [X, Y])).

puzzle_cell(X, Y, Puzzle) ->
    {Children, Class} =
        case maps:get({X, Y}, Puzzle) of
            0 -> {[], <<"input">>};
            Hint -> {[integer_to_binary(Hint)], <<"hint">>}
        end,
    ID = puzzle_cell_id(X, Y),
    #element{name = 'td', attributes = [{class, Class}, {id, ID}], children = Children}.

generate_grid(Parent) ->
    RandomGenerator = fun(X) -> (atomvm:random() rem X) + 1 end,
    {Puzzle, Solution} = sudoku_grid:parallel_random_puzzle(
        RandomGenerator, 25, 4, ?MAX_PROCESSING_TIME
    ),
    Parent ! {puzzle, Puzzle, Solution}.

find_next_value(Grid, Position, OldValue) ->
    Candidate = (OldValue + 1) rem 10,
    case sudoku_grid:is_move_valid(Grid, Position, Candidate) of
        true -> Candidate;
        false -> find_next_value(Grid, Position, Candidate)
    end.

puzzle_update_value_script({X, Y}, NewValue) ->
    ID = puzzle_cell_id(X, Y),
    Content =
        case NewValue of
            0 -> <<>>;
            _ -> integer_to_binary(NewValue)
        end,
    {Script, _} = dom:replace_children_script(<<"#", ID/binary>>, Content, 1),
    Script.

puzzle_solved_script(DeltaMS, HelpUses) ->
    SolvedCaptionL = case HelpUses of
        0 -> io_lib:format("Grid solved in ~Bs without using help", [DeltaMS div 1000]);
        1 -> io_lib:format("Grid solved in ~Bs with 1 use of help", [DeltaMS div 1000]);
        N -> io_lib:format("Grid solved in ~Bs with ~B uses of help", [DeltaMS div 1000, N])
    end,
    SolvedCaption = list_to_binary(SolvedCaptionL),
    {Script, _} = dom:replace_children_script(<<".sudoku-grid caption">>, SolvedCaption, 1),
    Script.

help_script(Grid, Solution) ->
    {Errors, Hints} = maps:fold(
        fun({X, Y}, Val, {AccErrors, AccHints}) ->
            case maps:get({X, Y}, Solution) of
                Val -> {AccErrors, AccHints};
                _OtherVal when Val =/= 0 -> {[{X, Y} | AccErrors], AccHints};
                OtherVal when Val =:= 0 -> {AccErrors, [{{X, Y}, OtherVal} | AccHints]}
            end
        end,
        {[], []},
        Grid
    ),
    case {Errors, Hints} of
        {[], []} ->
            {Script, _} = dom:replace_children_script(
                <<".sudoku-grid caption">>, <<"Grid is solved, cannot help further with it">>, 1
            ),
            {Script, Grid};
        {[], _} ->
            help_with(fun help_with_hint/2, Grid, Hints);
        {[_ | _], _} ->
            help_with(fun help_with_error/2, Grid, Errors)
    end.

help_with(Fun, Grid, Helps) ->
    [{_, FirstHelp} | _] = lists:sort([{atomvm:random(), Help} || Help <- Helps]),
    Fun(Grid, FirstHelp).

help_with_hint(Grid, {{X, Y}, Val}) ->
    Script = puzzle_update_value_script({X, Y}, Val),
    NewGrid = maps:put({X, Y}, Val, Grid),
    {Script, NewGrid}.

help_with_error(Grid, {X, Y}) ->
    Script = puzzle_update_value_script({X, Y}, 0),
    NewGrid = maps:put({X, Y}, 0, Grid),
    {Script, NewGrid}.
