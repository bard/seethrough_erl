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
-export([render/2, test/0]).
-compile([export_all]).

%-define(DEBUG(Message, Args), io:format("~s~n", [io_lib:format(Message, Args)])).
-define(DEBUG(Message, Args), nop).

%%%-------------------------------------------------------------------
%%% Sample environment
%%%-------------------------------------------------------------------
%%
%% env() ->
%%     [{title, "Space"},
%%      {alignment, "center"},
%%      {subtitle, {?MODULE, get_subtitle, []}},
%%      {background_color, "blue_skies"},
%%      {crew, {?MODULE, get_crew, []}}].
%%
%% get_subtitle() ->
%%     "The final frontier...".
%%
%% get_crew() ->
%%     [[{address, "kirk@enterprise.glx"},
%%       {name, "Jim"}],
%%      [{address, "spock@enterprise.glx"},
%%       {name, "Spock"}],
%%      [{address, "mccoy@enterprise.glx"},
%%       {name, "Doc"}]].

%%%-------------------------------------------------------------------
%%% Main
%%%-------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: render/3
%% Purpose: apply a template file to an environment, optionally within
%%          a layout (aka super-template).  The result of content
%%          rendering is available to the layout as "content".
%%
%% Example:
%%          render({file, "/tmp/content.html"}, [{name, "enterprise"}],
%%                 [{layout, "/tmp/layout.html"}]).
%%
%%          In /tmp/layout.html, where content should appear:
%%             <div e:replace="content"/>
%%--------------------------------------------------------------------

render({layout, Layout, [{content, Content}]}, Env) ->
    Tree = apply_template(Layout,
                          [{content, apply_template(Content, Env)} | Env]),
    xmerl:export_simple(lists:flatten([Tree]),
                        xmerl_xml,
                        [#xmlAttribute{name = prolog, value = ""}]);
render(Template, Env) ->
    Tree = apply_template(Template, Env),
    xmerl:export_simple(lists:flatten([Tree]),
                        xmerl_xml,
                        [#xmlAttribute{name = prolog, value = ""}]).


%%--------------------------------------------------------------------
%% Function: apply_template/2
%% Purpose: receives an term representing an XML parse, as output by
%%          xmerl_scan:file/1 and xmerl_scan:string/1, and returns a
%%          transformed XML tree.
%%----------------------------------------------------------------------

apply_template(Tree, Env) when is_record(Tree, xmlElement) ->
    Closures = compile(Tree),
    exec(Closures, Env);
apply_template({string, String}, Env) ->
    {#xmlElement{} = Tree, _} = xmerl_scan:string(String),
    apply_template(Tree, Env); 
apply_template({file, File}, Env) ->
    {#xmlElement{} = Tree, _} = xmerl_scan:file(File),
    apply_template(Tree, Env).


%%--------------------------------------------------------------------
%% Function: compile/1
%% Purpose: Compile the XML tree, transforming elements that need
%%          transformation.
%%--------------------------------------------------------------------

compile([Node | Rest]) ->
    NHead = compile(Node),
    NTail = compile(Rest),
    if
        is_list(NHead) ->
            NHead ++ NTail;
        true ->
            [ NHead | NTail ]
    end;

compile([]) ->
    [];

compile(Node) when is_record(Node, xmlElement) ->
    compile(Node, []);

compile(Node) ->
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

compile(Node = #xmlElement{attributes =
                         [#xmlAttribute{name = 'e:content',
                                        value = VarName} | Rest]},
      Attributes) ->
    fun(Env) ->
            ?DEBUG("Expanding e:content", []),
            {value, VarValue} = env_lookup(VarName, Env),
            Fun = compile(Node#xmlElement{content = [normalize_value(VarValue)],
                                          attributes = Rest},
                          Attributes),
            exec(Fun, Env)
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

compile(_Node = #xmlElement{attributes =
                          [#xmlAttribute{name = 'e:replace',
                                         value = VarName} | _RAttributes]},
      _Attributes) ->
    fun(Env) ->
            ?DEBUG("Expanding e:replace", []),
            {value, VarValue} = env_lookup(VarName, Env),
            normalize_value(VarValue)
    end;

%%---------------------------------------------------------------------
%% Just like replace but plugs in the results of template application.
%%---------------------------------------------------------------------

compile(_Node = #xmlElement{attributes =
                          [#xmlAttribute{name = 'e:include',
                                         value = FileName} | _RAttributes]},
      _Attributes) ->
    {#xmlElement{} = Tree, _Misc} = xmerl_scan:file(FileName),
    compile(Tree);

%%--------------------------------------------------------------------
%% Trasform an element with "e:condition" attribute into empty text
%% node (effectively removing it from the document) if the looked up
%% value pointed by the attribute is false or undefined.
%%--------------------------------------------------------------------

compile(Node = #xmlElement{attributes =
                         [#xmlAttribute{name = 'e:condition',
                                        value = VarName} | RAttributes]},
      Attributes) ->
    fun(Env) ->
            ?DEBUG("Expanding e:condition", []),
            case env_lookup(VarName, Env) of
                {value, false} ->
                    #xmlText{value = ""};
                {value, undefined} ->
                    #xmlText{value = ""};
                {value, _VarValue} ->
                    exec(compile(Node#xmlElement{attributes = RAttributes},
                                 Attributes), Env);
                undefined ->
                    #xmlText{value = ""}
            end
    end;

%%---------------------------------------------------------------------
%% e:repeat
%%---------------------------------------------------------------------

compile(Node = #xmlElement{attributes =
                         [#xmlAttribute{name = 'e:repeat',
                                        value = ContextName} | RAttributes]},
      Attributes) ->
    Closures = compile(Node#xmlElement{attributes = RAttributes},
                     Attributes),

    fun(Env) ->
            ?DEBUG("Expanding e:repeat", []),
            {value, CloneEnvs} = env_lookup(ContextName, Env),
            [ exec(Closures, E) || E <- CloneEnvs ]
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

compile(Node = #xmlElement{name = 'e:attr',
                         attributes = Attributes}, _Attributes) ->
    {value, AttrForName} = lists:keysearch(name, #xmlAttribute.name, Attributes),
    Name = list_to_atom(AttrForName#xmlAttribute.value),

    case lists:keysearch(value, #xmlAttribute.name, Attributes) of
        {value, AttrForValue} ->
            VarName = list_to_atom(AttrForValue#xmlAttribute.value),
            fun(Env) ->
                    ?DEBUG("Expanding e:attr", []),
                    {value, VarValue} = env_lookup(VarName, Env),
                    #xmlAttribute{name = Name, value = VarValue}
            end;
        false ->
            Closures = compile(Node#xmlElement.content),
            fun(Env) ->
                    ?DEBUG("Expanding e:attr", []),

                    Value =
                        lists:foldr(
                          fun(#xmlText{value = V}, Acc) ->
                                  [V|Acc]
                          end, [], exec(Closures, Env)),

                    #xmlAttribute{name = Name, value = Value}
            end
    end;

%%--------------------------------------------------------------------
%% Default behaviour
%%--------------------------------------------------------------------

compile(Node = #xmlElement{attributes = [Attr | Rest]}, Attributes) ->
    compile(Node#xmlElement{attributes = Rest}, [Attr | Attributes]);

compile(Node = #xmlElement{attributes = []}, Attributes) ->
    Closures = compile(Node#xmlElement.content),
    fun(Env) ->
            {ResultAttributes, ResultContent} =
                lists:partition(fun(N) -> is_record(N, xmlAttribute)
                                end,
                                exec(Closures, Env)),
            Node#xmlElement{attributes = Attributes ++
                            ResultAttributes,
                            content = ResultContent}
    end.


%%%-------------------------------------------------------------------
%%% Utilities
%%%-------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: normalize_value/1
%% Purpose: when input is an xmlElement, return it unmodified;
%%          otherwise, return it wrapped into an xmlText record.
%%--------------------------------------------------------------------

normalize_value(VarValue) when is_record(VarValue, xmlElement) ->
    VarValue;
normalize_value(VarValue) when is_record(VarValue, xmlText) ->
    VarValue;
normalize_value(VarValue) when is_integer(VarValue) ->
    normalize_value(integer_to_list(VarValue));
normalize_value(VarValue) ->
    #xmlText{value = VarValue}.


%%--------------------------------------------------------------------
%% Function: env_lookup/2
%% Purpose: Look up a variable in the given environment and return
%%          its value.
%%--------------------------------------------------------------------

env_lookup(VarName, Env) when is_list(VarName) ->
    env_lookup(list_to_atom(VarName), Env);
env_lookup(VarName, Env) ->
    ?DEBUG("Looking for ~p in ~p", [VarName, Env]),
    case proplists:get_value(VarName, Env) of
        undefined ->
            ?DEBUG("Environment lookup for '~p' failed.", [VarName]),
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

exec(Closures, Env) when is_list(Closures) ->
    lists:flatten([ Fun(Env) || Fun <- Closures]);

exec(Fun, Env) ->
    Fun(Env).

%%%-------------------------------------------------------------------
%%% Test suite
%%%-------------------------------------------------------------------

stringify(Tree) ->
    lists:flatten(
      xmerl:export_simple(lists:flatten([Tree]),
                          xmerl_xml,
                          [#xmlAttribute{name = prolog,value = ""}])).
    
test1() ->
    X = apply_template({string, "<title e:content=\"title\"/>"}, [{title, "title"}]),
    "<title>title</title>" = stringify(X).

test2() ->
    X = apply_template({string, "<span e:replace=\"subtitle\"/>"}, [{subtitle, "subtitle"}]),
    "subtitle" = stringify(X).

test3() ->
    X = apply_template({string, "<h2><e:attr name=\"style\">font-weight: bold;</e:attr></h2>"}, []),
    "<h2 style=\"font-weight: bold;\"/>" = stringify(X).

test4() ->
    X = apply_template({string, "<h2><e:attr name=\"align\"><span e:replace=\"alignment\"/></e:attr></h2>"},
                [{alignment, "center"}]),
    "<h2 align=\"center\"/>" = stringify(X).

test5() ->
    X = apply_template({string, "<h2><e:attr name=\"bgcolor\" value=\"color\"/></h2>"},
                [{color, "blue"}]),
    "<h2 bgcolor=\"blue\"/>" = stringify(X).

test6() ->
    X = apply_template({string, "<div e:condition=\"error\">Boom!</div>"}, [{error, false}]),
    "" = stringify(X),
    X1 = apply_template({string, "<div e:condition=\"error\">Boom!</div>"}, []),
    "" = stringify(X1),
    X2 = apply_template({string, "<div e:condition=\"error\">Boom!</div>"}, [{error, true}]),
    "<div>Boom!</div>" = stringify(X2).

test7() ->
    S = "<tbody><tr e:repeat=\"crew\"><td e:content=\"address\"/></tr></tbody>",
    F = fun() -> [[{address, "address"}]] end,
    X = apply_template({string, S}, [{crew, F}]),
    "<tbody><tr><td>address</td></tr></tbody>" = stringify(X).

test8() ->
    S = "<select><option e:repeat=\"names\">" ++
        "<span e:replace=\"name\"/>" ++
        "<e:attr name=\"value\" value=\"name\"/>" ++
        "<e:attr e:condition=\"current\" name=\"selected\">selected</e:attr>" ++
        "</option></select>",
    X = apply_template({string, S}, [{names, [[{name, "jim"}, {current, true}],
                                       [{name, "scotty"}]]}]),
    "<select><option value=\"jim\" selected=\"selected\">jim</option>" ++
        "<option value=\"scotty\">scotty</option></select>" = stringify(X).

test9() ->
    S = "<span e:content=\"foreign\"/>",
    {El, _} = xmerl_scan:string("<i>hello</i>"),
    X = apply_template({string, S}, [{foreign, El}]),
    "<span><i>hello</i></span>" = stringify(X).

test() ->
    test1(),
    test2(),
    test3(),
    test4(),
    test5(),
    test6(),
    test7(),
    test8(),
    test9(),
    ok.
