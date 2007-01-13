%%%-------------------------------------------------------------------
%%% File    : seethrough.erl
%%% Author  : Massimiliano Mirra <bard [at] hyperstruct [dot] net>
%%% Description : XML/XHTML templating engine
%%%
%%% Created :  25 Dec 2006 by Massimiliano Mirra
%%%            <bard [at] hyperstruct [dot] net>
%%% License :
%%%
%%%  Copyright (C) 2006-2007 by Massimiliano Mirra
%%%
%%%  This program is free software; you can redistribute it and/or
%%%  modify it under the terms of the GNU General Public License as
%%%  published by the Free Software Foundation; either version 2 of
%%%  the License, or (at your option) any later version.
%%%
%%%  This program is distributed in the hope that it will be useful,
%%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%  GNU General Public License for more details.
%%%
%%%  You should have received a copy of the GNU General Public License
%%%  along with this program; if not, write to the Free Software
%%%  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
%%%  02110-1301 USA
%%%
%%%-------------------------------------------------------------------

-module(seethrough).

-include("/usr/lib/erlang/lib/xmerl-1.0.5/include/xmerl.hrl").
-compile(export_all).

%%%-------------------------------------------------------------------
%%% Example
%%%-------------------------------------------------------------------

test() ->
    io:format(
      apply_template("test.html", 
          [{title, "Space"},
           {alignment, "center"},
           {subtitle, {?MODULE, get_subtitle, []}},
           {background_color, "blue_skies"},
           {crew, {? MODULE, get_crew, []}}])).

get_subtitle() ->
    "The last frontier...".

get_crew() ->
    [[{address, "kirk@enterprise.glx"},
      {name, "Jim"}],
     [{address, "spock@enterprise.glx"},
      {name, "Spock"}],
     [{address, "mccoy@enterprise.glx"},
      {name, "Doc"}]].


%%%-------------------------------------------------------------------
%%% Main
%%%-------------------------------------------------------------------

apply_template(File, Env) ->
    {Tree, _Misc} = xmerl_scan:file(File),
    xmerl:export_simple(
      [visit(Tree, Env)], xmerl_xml).

%%--------------------------------------------------------------------
%% Function: visit/2
%% Purpose: Visit the XML tree, transforming the elements that need
%%          transformation.
%%--------------------------------------------------------------------

visit([Node | Rest], Env) ->
    NHead = visit(Node, Env),
    NTail = visit(Rest, Env),
    if
        is_list(NHead) ->
            NHead ++ NTail;
        true ->
            [ NHead | NTail ]
    end;
visit([], _Env) ->
    [];    
visit(Node, Env) when is_record(Node, xmlElement) ->
    visit(Node, [], Env);
visit(Node, _Env) when is_record(Node, xmlText) ->
    Node; 
visit(Node, _Env) ->
    Node.

%%--------------------------------------------------------------------
%% Function: visit/3
%% Purpose: Transforms an XML element.
%%--------------------------------------------------------------------

visit(Node = #xmlElement{attributes =
                         [#xmlAttribute{name = 'e:content',
                                        value = VarName} | Rest]},
      Attributes, Env) ->
    {value, VarValue} = env_lookup(VarName, Env),
    visit(Node#xmlElement{content = [#xmlText{value = VarValue}],
                          attributes = Rest},
          Attributes, Env);

%%--------------------------------------------------------------------
%% Transform an element with a "e:replace" attribute to a text node
%% with value looked up in the environment.
%%
%% For example:
%%
%%   My name is <span e:replace="name"/>.
%%
%% If in the environment "name" is "Jim", it will be transformed to:
%%
%%   My name is Jim.
%%--------------------------------------------------------------------

visit(_Node = #xmlElement{attributes =
                         [#xmlAttribute{name = 'e:replace',
                                        value = VarName} | _RAttributes]},
      _Attributes, Env) ->
    {value, VarValue} = env_lookup(VarName, Env),
    #xmlText{value = VarValue};

%%--------------------------------------------------------------------
%% Transform an element with a "e:content" attribute to an equal
%% element having the value looked up in the environment as content.
%%
%% For example:
%%
%%   My name is <span class="font-weight: bold;" e:replace="name"/>.
%%
%% If in the environment name" is "Jim", it will be transformed to:
%%
%%   My name is <span class="font-weight: bold;">Jim</span>.
%%--------------------------------------------------------------------

visit(Node = #xmlElement{attributes =
                         [#xmlAttribute{name = 'e:repeat',
                                        value = ContextName} | RAttributes]},
      Attributes, Env) ->
    {value, CloneEnvs} = env_lookup(ContextName, Env),

    [ visit(Node#xmlElement{attributes = RAttributes}, Attributes, CloneEnv)
      || CloneEnv <- CloneEnvs ];

%%--------------------------------------------------------------------
%% Transform an <e:attr> element into an attribute that will be
%% received by the parent element.
%%
%% The "name" attribute of the <e:attr> element tells the name of the
%% attribute that will be set in the parent.  For example, an element
%% such as:
%%
%%   <div><e:attr name="class"/></div>
%%
%% Will cause the parent <div> to acquire a "class" attribute.
%%
%% The value of the attribute can be specified via the content of the
%% <e:attr> element.  For example, in:
%%
%%   <div><e:attr name="class">shiny</e:attr></div>
%%
%% The <div> will acquire the "class" attribute with the "shiny"
%% value:
%%
%%   <div class="shiny"/>
%%
%% The content of the <e:attr> element is transformed as usual.  For
%% example, in:
%%
%%   <div><e:attr name="class"><span e:replace="cl"/></e:attr></div>
%%
%% The content of <e:attr> will be given by the transformation of
%% <span e:replace="cl"/>, which in turn is given by the environment
%% lookup for the "cl" value.  If the "cl" value is "dark", the first
%% transformation will be:
%%
%%   <div><e:attr name="class">dark</e:attr></div>
%%
%% And the next one:
%%
%%   <div class="dark"/>
%%
%% As a shortcut, non-complex values can be passed via a "value"
%% attribute in the <e:attr> element instead of its content.  The
%% following is equivalent to the previous example:
%%
%%   <div><e:attr name="class" value="cl"/></div>
%% 
%%--------------------------------------------------------------------

visit(Node = #xmlElement{name = 'e:attr',
                         attributes = Attributes}, _Attributes, Env) ->
    {value, AttrForName} = lists:keysearch(name, #xmlAttribute.name, Attributes),
    Name = list_to_atom(AttrForName#xmlAttribute.value),

    Value = 
        case lists:keysearch(value, #xmlAttribute.name, Attributes) of
            {value, AttrForValue} ->
                VarName = list_to_atom(AttrForValue#xmlAttribute.value),
                {value, VarValue} = env_lookup(VarName, Env),
                VarValue;
            false ->
                [Content] = visit(Node#xmlElement.content, Env),
                Content#xmlText.value
        end,
    #xmlAttribute{name = Name, value = Value};

visit(Node = #xmlElement{attributes = [Attr | Rest]}, Attributes, Env) ->
    visit(Node#xmlElement{attributes = Rest}, [Attr | Attributes], Env);

visit(Node = #xmlElement{attributes = []}, Attributes, Env) ->
    {ResultAttributes, ResultContent} = 
        lists:partition(fun(N) -> is_record(N, xmlAttribute) end,
                        visit(Node#xmlElement.content, Env)),    
    Node#xmlElement{attributes = Attributes ++ ResultAttributes,
                    content = ResultContent}.



%%%-------------------------------------------------------------------
%%% Utilities
%%%-------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: env_lookup/3
%% Purpose: Look up a variable in the given environment and return
%%          its value.
%%--------------------------------------------------------------------

env_lookup(VarName, Env) when is_list(VarName) ->
    env_lookup(list_to_atom(VarName), Env);
env_lookup(VarName, Env) ->
    case proplists:get_value(VarName, Env) of
        undefined ->
            {value, "ENV ERROR"};
        {Module, FunName, Args} ->
            {value, apply(Module, FunName, Args)};
        Value ->
            {value, Value}
    end.
