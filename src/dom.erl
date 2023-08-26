% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(dom).

-export([
    append_child_script/3, replace_children_script/3, replace_with_script/3, create_element_script/2
]).

-include("dom.hrl").

%% @doc Generate a script to call appendChild method on an element with another
%% element
-spec append_child_script(iodata(), dom_element() | iodata(), pos_integer()) ->
    {iodata(), pos_integer()}.
append_child_script(QuerySelector, Element, ElementVarID) ->
    element_method(QuerySelector, <<"appendChild">>, Element, ElementVarID).

%% @doc Generate a script to call replaceChildren method on an element with
%% another element
-spec replace_children_script(iodata(), dom_element() | iodata(), pos_integer()) ->
    {iodata(), pos_integer()}.
replace_children_script(QuerySelector, Element, ElementVarID) ->
    element_method(QuerySelector, <<"replaceChildren">>, Element, ElementVarID).

%% @doc Generate a script to call replaceWith method on an element with
%% another element
-spec replace_with_script(iodata(), dom_element() | iodata(), pos_integer()) ->
    {iodata(), pos_integer()}.
replace_with_script(QuerySelector, Element, ElementVarID) ->
    element_method(QuerySelector, <<"replaceWith">>, Element, ElementVarID).

element_method(QuerySelector, Method, Element, ElementVarID) ->
    case is_tuple(Element) of
        true ->
            {CreateElementScript, NewElementVarID} = create_element_script(Element, ElementVarID),
            Parameter = [<<"e">>, integer_to_list(ElementVarID)];
        false ->
            CreateElementScript = [],
            NewElementVarID = ElementVarID,
            Parameter = [<<"\"">>, escape(Element), <<"\"">>]
    end,
    AppendScript = [
        CreateElementScript,
        <<"document.querySelector(\"">>,
        escape(QuerySelector),
        <<"\").">>,
        Method,
        <<"(">>,
        Parameter,
        <<");">>
    ],
    {AppendScript, NewElementVarID}.

%% @doc Generate a script to call createElement method with an element
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
