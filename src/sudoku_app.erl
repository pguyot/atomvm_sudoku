% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(sudoku_app).

-export([start/2, stop/1]).

start(_Type, Args) ->
    sudoku_sup:start(Args).

stop(_State) ->
    ok.
