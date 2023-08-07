% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(dom_test).
-include_lib("eunit/include/eunit.hrl").

-include("dom.hrl").

create_element_script_test() ->
    {CreateScript, NextID} = dom:create_element_script(
        #element{name = h1, children = [{text, <<"Hello DOM">>}]}, 1
    ),
    ?assertEqual(2, NextID),
    ?assertEqual(
        <<"const e1=document.createElement(\"h1\");e1.append(\"Hello DOM\");">>,
        iolist_to_binary(CreateScript)
    ).

create_element_script_escape_test() ->
    {CreateScript, NextID} = dom:create_element_script(
        #element{name = h1, children = [{text, <<"42\"\n">>}]}, 1
    ),
    ?assertEqual(2, NextID),
    ?assertEqual(
        <<"const e1=document.createElement(\"h1\");e1.append(\"42\\\"\n\");">>,
        iolist_to_binary(CreateScript)
    ).

create_element_script_attribute_test() ->
    {CreateScript, NextID} = dom:create_element_script(
        #element{name = h1, attributes = [{ns, <<"namespace">>}, {<<"data-escape">>, <<"42\"">>}]},
        1
    ),
    ?assertEqual(2, NextID),
    ?assertEqual(
        <<"const e1=document.createElement(\"h1\");e1.setAttribute(\"ns\",\"namespace\");e1.setAttribute(\"data-escape\",\"42\\\"\");">>,
        iolist_to_binary(CreateScript)
    ).

create_element_script_child_test() ->
    {CreateScript, NextID} = dom:create_element_script(
        #element{
            name = p,
            children = [
                <<"Hello ">>, #element{name = span, children = [{text, <<"world">>}]}, <<".">>
            ]
        },
        1
    ),
    ?assertEqual(3, NextID),
    ?assertEqual(
        <<"const e1=document.createElement(\"p\");e1.append(\"Hello \");const e2=document.createElement(\"span\");e2.append(\"world\");e1.append(e2);e1.append(\".\");">>,
        iolist_to_binary(CreateScript)
    ).
