% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(sudoku_sup).

-export([start/1, init/1]).

start(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init(_Args) ->
    ChildSpecs =
        [
            {
                sudoku_main,
                {sudoku_main, start_link, []},
                permanent,
                brutal_kill,
                worker,
                [sudoku_main]
            }
        ],
    SupFlags = {one_for_one, 1, 1},
    {ok, {
        SupFlags,
        ChildSpecs
    }}.
