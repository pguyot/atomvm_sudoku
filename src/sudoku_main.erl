% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(sudoku_main).

-export([start_link/0]).

-behavior(gen_server).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2]).

-include("dom.hrl").

start_link() ->
    gen_server:start_link(?MODULE, [], []).

-record(state, {}).

init([]) ->
    {CreateScript, _} = dom:create_element_script(
        #element{
            name = 'div',
            children = [
                #element{name = h1, children = [<<"Hello DOM">>]},
                #element{
                    name = p,
                    children = [
                        <<"A ">>,
                        #element{
                            name = span,
                            attributes = [{style, <<"color: green">>}],
                            children = [<<"nice">>]
                        },
                        <<" world.">>
                    ]
                }
            ]
        },
        1
    ),
    emscripten:run_script(
        [
            CreateScript,
            <<"const root = document.querySelector('#root');">>,
            <<"root.appendChild(e1);">>
        ],
        [main_thread, async]
    ),
    {ok, #state{}}.

handle_cast(_Msg, #state{} = State0) ->
    {noreply, State0}.

handle_call(_Request, _From, State) ->
    {reply, {error, unimplemented}, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, #state{}) ->
    ok.
