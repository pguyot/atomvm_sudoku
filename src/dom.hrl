% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-record(element, {
    name :: iodata() | atom(),
    attributes = [] :: [dom_attribute()],
    children = [] :: [dom_node()]
}).

-type dom_node() :: text_node() | dom_element().
-type dom_element() :: #element{}.
-type dom_attribute() :: {Name :: iodata() | atom(), Value :: iodata()}.
-type text_node() :: {text, iodata()} | unicode:unicode_binary().
