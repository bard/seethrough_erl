%%%-------------------------------------------------------------------
%%% File    : seethrough.erl
%%% Author  : Massimiliano Mirra <bard [at] hyperstruct [dot] net>
%%% Description : XML/XHTML templating engine
%%% License : BSD
%%%
%%% Copyright (c) 2006-2007 Massimiliano Mirra.
%%%
%%% Contributors:
%%%
%%%   Joel Reymont <joelr1 [at] gmail [dot] com>
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%%
%%%   * Redistributions of source code must retain the above copyright
%%%     notice, this list of conditions and the following disclaimer.
%%%
%%%   * Redistributions in binary form must reproduce the above
%%%     copyright notice, this list of conditions and the following
%%%     disclaimer in the documentation and/or other materials provided
%%%     with the distribution.
%%%
%%%   * Neither the names of the copyright holders, nor the names of its
%%%     contributors may be used to endorse or promote products derived
%%%     from this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%%% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%% OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
%%% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%-------------------------------------------------------------------

-module(seethrough).

-include("xmerl.hrl").
-compile([export_all]).

%%%-------------------------------------------------------------------
%%% Example
%%%-------------------------------------------------------------------

env() ->
    [{title, "Space"},
     {alignment, "center"},
     {subtitle, {?MODULE, get_subtitle, []}},
     {background_color, "blue_skies"},
     {crew, {? MODULE, get_crew, []}}].

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
    Closures = visit(Tree),
    Tree1 = render(Closures, Env),
    xmerl:export_simple(lists:flatten([Tree1]), xmerl_xml,
                        [#xmlAttribute{name = prolog, value = ""}]).

%%--------------------------------------------------------------------
%% Function: visit/1
%% Purpose: Visit the XML tree, transforming the elements that need
%%          transformation.
%%--------------------------------------------------------------------

visit([Node | Rest]) ->
    NHead = visit(Node),
    NTail = visit(Rest),
    if
        is_list(NHead) ->
            NHead ++ NTail;
        true ->
            [ NHead | NTail ]
    end;

visit([]) ->
    [];

visit(Node) when is_record(Node, xmlElement) ->
    visit(Node, []);

visit(Node) ->
    fun(_) -> Node end.

%%--------------------------------------------------------------------
%% Transform an element with a "e:content" attribute to an equal
%% element having the value looked up in the environment as content.
%%
%% For example:
%%
%%   My name is <span class="font-weight: bold;" e:content="name"/>.
%%
%% If in the environment name" is "Jim", it will be transformed to:
%%
%%   My name is <span class="font-weight: bold;">Jim</span>.
%%--------------------------------------------------------------------

visit(Node = #xmlElement{attributes =
                         [#xmlAttribute{name = 'e:content',
                                        value = VarName} | Rest]},
      Attributes) ->
    fun(Env) ->
            {value, VarValue} = env_lookup(VarName, Env),
            Fun = visit(Node#xmlElement{content = [#xmlText{value = VarValue}],
                                        attributes = Rest},
                        Attributes),
            render(Fun, Env)
    end;

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
      _Attributes) ->
    fun(Env) ->
            {value, VarValue} = env_lookup(VarName, Env),
            #xmlText{value = VarValue}
    end;

%%---------------------------------------------------------------------
%% Just like replace but plugs in the results of template application.
%%---------------------------------------------------------------------

visit(_Node = #xmlElement{attributes =
                          [#xmlAttribute{name = 'e:include',
                                         value = FileName} | _RAttributes]},
      _Attributes) ->
    {Tree, _Misc} = xmerl_scan:file(FileName),
    visit(Tree);

%%--------------------------------------------------------------------
%% Trasform an element with "e:condition" attribute into empty text
%% node (effectively removing it from the document) if the looked up
%% value pointed by the attribute is false or undefined.
%%--------------------------------------------------------------------

visit(Node = #xmlElement{attributes =
                         [#xmlAttribute{name = 'e:condition',
                                        value = VarName} | RAttributes]},
      Attributes) ->
    fun(Env) ->
            case env_lookup(VarName, Env) of
                {value, false} ->
                    #xmlText{value = ""};
                {value, undefined} ->
                    #xmlText{value = ""};
                {value, _VarValue} ->
                    render(visit(Node#xmlElement{attributes = RAttributes},
                                 Attributes), Env);
                undefined ->
                    #xmlText{value = ""}
            end
    end;

%%---------------------------------------------------------------------
%% e:repeat
%%---------------------------------------------------------------------

visit(Node = #xmlElement{attributes =
                         [#xmlAttribute{name = 'e:repeat',
                                        value = ContextName} | RAttributes]},
      Attributes) ->
    Closures = visit(Node#xmlElement{attributes = RAttributes},
                     Attributes),
    fun(Env) ->
            {value, CloneEnvs} = env_lookup(ContextName, Env),
            [ render(Closures, E) || E <- CloneEnvs ]
    end;

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
                         attributes = Attributes}, _Attributes) ->
    {value, AttrForName} = lists:keysearch(name, #xmlAttribute.name,
                                           Attributes),
    Name = list_to_atom(AttrForName#xmlAttribute.value),

    case lists:keysearch(value, #xmlAttribute.name, Attributes) of
        {value, AttrForValue} ->
            VarName = list_to_atom(AttrForValue#xmlAttribute.value),
            fun(Env) ->
                    {value, VarValue} = env_lookup(VarName, Env),
                    #xmlAttribute{name = Name, value = VarValue}
            end;
        false ->
            Closures = visit(Node#xmlElement.content),
            fun(Env) ->
                    [Content] = render(Closures, Env),
                    Value = Content#xmlText.value,
                    #xmlAttribute{name = Name, value = Value}
            end
    end;

%%--------------------------------------------------------------------

visit(Node = #xmlElement{attributes = [Attr | Rest]}, Attributes) ->
    visit(Node#xmlElement{attributes = Rest}, [Attr | Attributes]);

visit(Node = #xmlElement{attributes = []}, Attributes) ->
    Closures = visit(Node#xmlElement.content),
    fun(Env) ->
            {ResultAttributes, ResultContent} =
                lists:partition(fun(N) -> is_record(N, xmlAttribute)
                                end,
                                render(Closures, Env)),
            Node#xmlElement{attributes = Attributes ++
                            ResultAttributes,
                            content = ResultContent}
    end.

%%%-------------------------------------------------------------------
%%% Utilities
%%%-------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: env_lookup/2
%% Purpose: Look up a variable in the given environment and return
%%          its value.
%%--------------------------------------------------------------------

env_lookup(VarName, Env) when is_list(VarName) ->
    env_lookup(list_to_atom(VarName), Env);
env_lookup(VarName, Env) ->
    case proplists:get_value(VarName, Env) of
        undefined ->
            undefined;
        {Module, FunName, Args} ->
            {value, apply(Module, FunName, Args)};
        F when is_function(F) ->
            {value, F()};
        Value ->
            {value, Value}
    end.

%%---------------------------------------------
%% Evaluate closures using a given environment
%%---------------------------------------------

render(Closures, Env) when is_list(Closures) ->
    lists:flatten([ Fun(Env) || Fun <- Closures]);

render(Fun, Env) ->
    Fun(Env).

%%%-------------------------------------------------------------------
%%% Test suite
%%%-------------------------------------------------------------------

process(String, Env) ->
    {Tree, _} = xmerl_scan:string(String),
    Closures = visit(Tree),
    Tree1 = render(Closures, Env),
    Tree2 = lists:flatten([Tree1]),
    XML = xmerl:export_simple(Tree2, xmerl_xml,
                              [#xmlAttribute{name = prolog,value = ""}]),
    lists:flatten(XML).

test1() ->
    X = process("<title e:content=\"title\"/>", [{title, "title"}]),
    "<title>title</title>" = X.

test2() ->
    X = process("<span e:replace=\"subtitle\"/>", [{subtitle, "subtitle"}]),
    "subtitle" = X.

test3() ->
    X = process("<h2><e:attr name=\"style\">font-weight: bold;</e:attr></h2>", []),
     "<h2 style=\"font-weight: bold;\"/>" = X.

test4() ->
    X = process("<h2><e:attr name=\"align\"><span e:replace=\"alignment\"/></e:attr></h2>",
                [{alignment, "center"}]),
    "<h2 align=\"center\"/>" = X.

test5() ->
    X = process("<h2><e:attr name=\"bgcolor\" value=\"color\"/></h2>",
                [{color, "blue"}]),
    "<h2 bgcolor=\"blue\"/>" = X.

test6() ->
    X = process("<div e:condition=\"error\">Boom!</div>", [{error, false}]),
    "" = X,
    X1 = process("<div e:condition=\"error\">Boom!</div>", []),
    "" = X1,
    X2 = process("<div e:condition=\"error\">Boom!</div>", [{error, true}]),
    "<div>Boom!</div>" = X2.

test7() ->
    S = "<tbody><tr e:repeat=\"crew\"><td e:content=\"address\"/></tr></tbody>",
     F = fun() -> [[{address, "address"}]] end,
    X = process(S, [{crew, F}]),
    "<tbody><tr><td>address</td></tr></tbody>" = X.

test() ->
    test1(),
    test2(),
    test3(),
    test4(),
    test5(),
    test6(),
    test7(),
    ok.
