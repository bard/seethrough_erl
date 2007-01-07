%%%-------------------------------------------------------------------
%%% File    : seethrough.erl
%%% Author  : Massimiliano Mirra <bard [at] hyperstruct [dot] net>
%%% Description : XML/XHTML templating engine
%%%
%%% Created :  25 Dec 2006 by Massimiliano Mirra
%%%            <bard [at] hyperstruct [dot] net>
%%%-------------------------------------------------------------------
-module(seethrough).

-include("/usr/lib/erlang/lib/xmerl-1.0.5/include/xmerl.hrl").
-compile(export_all).

%%% ----------------------------------------------------------------------
%%% Example
%%% ----------------------------------------------------------------------

test() ->
    io:format(
      apply_template("test.html", 
          [{title, "Space"},
           {subtitle, {?MODULE, get_subtitle, []}},
           {crew, {?MODULE, get_crew, []}}])).

get_subtitle() ->
    "The last frontier...".

get_crew() ->
    [[{address, "kirk@enterprise.glx"},
      {name, "Jim"}],
     [{address, "spock@enterprise.glx"},
      {name, "Spock"}],
     [{address, "mccoy@enterprise.glx"},
      {name, "Doc"}]].


%%% ----------------------------------------------------------------------
%%% Main
%%% ----------------------------------------------------------------------

apply_template(File, Env) ->
    {Tree, _Misc} = xmerl_scan:file(File),
    xmerl:export_simple(
      [visit(Tree, Env)], xmerl_xml).


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

visit(Node = #xmlElement{attributes =
                         [#xmlAttribute{name = 'e:content',
                                        value = VarName} | Rest]},
      Attributes, Env) ->
    {value, VarValue} = env_lookup(VarName, Env),
    visit(Node#xmlElement{content = [#xmlText{value = VarValue}],
                          attributes = Rest},
          Attributes, Env);

visit(_Node = #xmlElement{attributes =
                         [#xmlAttribute{name = 'e:replace',
                                        value = VarName} | _RAttributes]},
      _Attributes, Env) ->
    {value, VarValue} = env_lookup(VarName, Env),
    #xmlText{value = VarValue};

visit(Node = #xmlElement{attributes =
                         [#xmlAttribute{name = 'e:repeat',
                                        value = ContextName} | RAttributes]},
      Attributes, Env) ->
    {value, CloneEnvs} = env_lookup(ContextName, Env),

    [ visit(Node#xmlElement{attributes = RAttributes}, Attributes, CloneEnv)
      || CloneEnv <- CloneEnvs ];

visit(Node = #xmlElement{attributes = [Attr | Rest]}, Attributes, Env) ->
    visit(Node#xmlElement{attributes = Rest}, [Attr | Attributes], Env);

visit(Node = #xmlElement{attributes = []}, Attributes, Env) ->
    Node#xmlElement{attributes = Attributes,
                    content = visit(Node#xmlElement.content, Env)}.


%%% ----------------------------------------------------------------------
%%% Utilities
%%% ----------------------------------------------------------------------

env_lookup(VarName, Env) when is_list(VarName) ->
    env_lookup(list_to_atom(VarName), Env);
env_lookup(VarName, Env) ->
    case lists:keysearch(VarName, 1, Env) of
        {value, {_Key, {Module, FunName, Args}}} ->
            {value, apply(Module, FunName, Args)};
        {value, {_Key, Value}} ->
            {value, Value};
        _Else ->
            {value, "ENV ERROR"}
    end.

