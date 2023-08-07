% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(dom).

-export([create_element_script/2]).

-include("dom.hrl").

-spec create_element_script(dom_element(), pos_integer()) -> {iodata(), pos_integer()}.
create_element_script(
    #element{name = ElementName, attributes = Attributes, children = Children}, ElementVarID
) ->
    CreateScript = [
        <<"const e">>,
        integer_to_list(ElementVarID),
        <<"=document.createElement(\"">>,
        escape(ElementName),
        <<"\");">>
    ],
    AttributeScript = [
        [
            <<"e">>,
            integer_to_list(ElementVarID),
            <<".setAttribute(\"">>,
            escape(AttributeName),
            <<"\",\"">>,
            escape(AttributeValue),
            <<"\");">>
        ]
     || {AttributeName, AttributeValue} <- Attributes
    ],
    {CreateChildrenScripts, NextID} = lists:foldl(
        fun(Child, {AccS, AccID}) ->
            {ChildScript, NewAccID} =
                case Child of
                    {text, Text} ->
                        {
                            [
                                <<"e">>,
                                integer_to_list(ElementVarID),
                                <<".append(\"">>,
                                escape(Text),
                                <<"\");">>
                            ],
                            AccID
                        };
                    Text when is_binary(Text) ->
                        {
                            [
                                <<"e">>,
                                integer_to_list(ElementVarID),
                                <<".append(\"">>,
                                escape(Text),
                                <<"\");">>
                            ],
                            AccID
                        };
                    #element{} = ChildNode ->
                        {ChildCreateScript, ChildNextID} = create_element_script(ChildNode, AccID),
                        {
                            [
                                ChildCreateScript,
                                <<"e">>,
                                integer_to_list(ElementVarID),
                                <<".append(e">>,
                                integer_to_list(AccID),
                                <<");">>
                            ],
                            ChildNextID
                        }
                end,
            {[ChildScript | AccS], NewAccID}
        end,
        {[], ElementVarID + 1},
        Children
    ),
    {[CreateScript, AttributeScript, lists:reverse(CreateChildrenScripts)], NextID}.

escape(Atom) when is_atom(Atom) ->
    escape(atom_to_list(Atom));
escape(List) when is_list(List) ->
    escape(list_to_binary(List));
escape(Bin) when is_binary(Bin) ->
    escape(binary_to_list(Bin), []).

escape([], Acc) ->
    lists:reverse(Acc);
escape([$" | Tail], Acc) ->
    escape(Tail, [$", $\\ | Acc]);
escape([H | Tail], Acc) ->
    escape(Tail, [H | Acc]).
