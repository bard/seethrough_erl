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
-export([render/2, render/3,
         compile/1, exec/2,
         apply_template/2, apply_template/3,
         get_attr_value/2,
         lookup/3]).

%-define(DEBUG(Message, Args), io:format("~s~n", [io_lib:format(Message, Args)])).
-define(DEBUG(Message, Args), no_op).

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
%% Function: render/2, render/3
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

render(Template, Env) ->
    render(Template, Env, []).

render(Template, Env, Handlers) ->
    Tree = render1(Template, Env, Handlers),
    xmerl:export_simple(lists:flatten([Tree]),
                        xmerl_xml,
                        [#xmlAttribute{name = prolog, value = ""}]).

render1({layout, Layout, [{content, Content}]}, Env, Handlers) ->
    apply_template(Layout,
                   [{content, apply_template(Content, Env, Handlers)} | Env],
                   Handlers);
render1(Template, Env, Handlers) ->
    apply_template(Template, Env, Handlers).


%%--------------------------------------------------------------------
%% Function: apply_template/2, apply_template/3
%% Purpose: receives a term representing an XML parse, as output by
%%          xmerl_scan:file/1 and xmerl_scan:string/1, and returns a
%%          transformed XML tree.
%%----------------------------------------------------------------------

apply_template(Tree, Env) ->
    apply_template(Tree, Env, []).

apply_template({string, String}, Env, Handlers) ->
    {#xmlElement{} = Tree, _} = xmerl_scan:string(String),
    apply_template(Tree, Env, Handlers); 
apply_template({file, File}, Env, Handlers) ->
    {#xmlElement{} = Tree, _} = xmerl_scan:file(File),
    apply_template(Tree, Env, Handlers);
apply_template(Tree, Env, Handlers) when is_record(Tree, xmlElement) ->
    Closures = dynvar:with([{handlers, Handlers}],
                           fun() -> compile(Tree) end),
    exec(Closures, Env).


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
            
            VarValue = lookup(first, VarName, Env),
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
            VarValue = lookup(first, VarName, Env),
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
            case lookup(first, VarName, Env) of
                false ->
                    #xmlText{value = ""};
                undefined ->
                    #xmlText{value = ""};
                _Value ->
                    exec(compile(Node#xmlElement{attributes = RAttributes},
                                 Attributes), Env)
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
            CloneEnvs = lookup(all, ContextName, Env),
            [exec(Closures, E) || {_C, E} <- CloneEnvs]
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

    TargetAttrName = get_attr_value(name, Attributes),
    case get_attr_value(value, Attributes) of
        undefined ->
            Closures = compile(Node#xmlElement.content),
            fun(Env) ->
                    ?DEBUG("Expanding e:attr", []),

                    TargetAttrValue =
                        lists:foldr(
                          fun(#xmlText{value = V}, Acc) ->
                                  [V|Acc]
                          end, [], exec(Closures, Env)),

                    #xmlAttribute{name = list_to_atom(TargetAttrName),
                                  value = TargetAttrValue}
            end;        
        VarName ->
            fun(Env) ->
                    ?DEBUG("Expanding e:attr", []),
                    
                    TargetAttrValue = lookup(first, list_to_atom(VarName), Env),
                    
                    #xmlAttribute{name = list_to_atom(TargetAttrName),
                                  value = TargetAttrValue}
            end
    end;

%%--------------------------------------------------------------------
%% Utility tag.  Output current environment.
%%--------------------------------------------------------------------

compile(_Node = #xmlElement{name = 'e:debug',
                          attributes = Attributes}, _Attributes) ->
    DebugTarget = get_attr_value(show, Attributes),
    case DebugTarget of
        "env" ->
            fun(Env) ->
                    #xmlText{value = io_lib:format("~p", [Env])}
            end
    end;


%%--------------------------------------------------------------------
%% Default behaviour
%%--------------------------------------------------------------------

%% Iterate on attributes.

compile(Node = #xmlElement{attributes = [Attr | Rest]}, Attributes) ->
    compile(Node#xmlElement{attributes = Rest}, [Attr | Attributes]);

%% No more attributes, proceed to element name.  Check namespace and
%% possibly invoke external handler, otherwise process internally.

compile(Node = #xmlElement{attributes = [],
                           nsinfo = NamespaceInfo,
                           namespace = NamespaceData,
                           content = Content},
        Attributes) ->

    Handler = 
        case NamespaceInfo of
            {Prefix, _Name} ->
                NamespaceURI = proplists:get_value(Prefix, NamespaceData#xmlNamespace.nodes),
                proplists:get_value(NamespaceURI, dynvar:fetch(handlers));
            [] ->
                undefined;
            undefined ->
                undefined
        end,

    case Handler of
        undefined ->
            Closures = compile(Content),
            fun(Env) ->
                    Results = exec(Closures, Env),

                    {ResultAttributes, ResultContents} =
                        lists:partition(fun(N) -> is_record(N, xmlAttribute) end, Results),

                    Node#xmlElement{attributes = Attributes ++ ResultAttributes,
                                    content = ResultContents}
            end;
        _ ->
            Handler:compile(Node, Attributes)
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

%%---------------------------------------------
%% Evaluate closures using a given environment
%%---------------------------------------------

exec(Closures, Env) when is_list(Closures) ->
    lists:flatten([ Fun(Env) || Fun <- Closures]);

exec(Fun, Env) ->
    Fun(Env).

%%---------------------------------------------
%% Given a list af attribute records, search
%% for the attribute with the given name and
%% return its value.
%%---------------------------------------------

get_attr_value(AttrName, Attributes) ->
    case lists:keysearch(AttrName, #xmlAttribute.name, Attributes) of
        {value, Attribute} ->
            Attribute#xmlAttribute.value;
        false ->
            undefined
    end.




lookup(first, Path, Env) ->
    case env:lookup(first, Path, Env) of
        undefined         -> undefined;
        {_VarName, Value} -> Value
    end;
lookup(all, Path, Env) ->
    env:lookup(all, Path, Env).
