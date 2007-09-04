-module(seethrough_test).

-include("xmerl.hrl").
-compile([export_all]).


%%%-------------------------------------------------------------------
%%% Runner
%%%-------------------------------------------------------------------

run() ->
    Exports = proplists:get_value(exports, ?MODULE:module_info()),
    Tests = [Function || {Function, 0} <- Exports,
                         string:str(atom_to_list(Function), "test") == 1],
    lists:map(
      fun(Test) ->
              {Test, case catch ?MODULE:Test() of
                         {'EXIT', Reason} -> {'FAIL', Reason};
                         _ -> ok
                     end}
      end, Tests).


%%%-------------------------------------------------------------------
%%% Tests
%%%-------------------------------------------------------------------

test_content() ->
    X = seethrough:apply_template({string, "<title e:content=\"title\"/>"}, [{title, "title"}]),
    "<title>title</title>" = stringify(X).

test_replace() ->
    X = seethrough:apply_template({string, "<span e:replace=\"subtitle\"/>"}, [{subtitle, "subtitle"}]),
    "subtitle" = stringify(X).

test_attr_with_immediate_value() ->
    X = seethrough:apply_template({string, "<h2><e:attr name=\"style\">font-weight: bold;</e:attr></h2>"}, []),
    "<h2 style=\"font-weight: bold;\"/>" = stringify(X).

test_attr_with_computed_content() ->
    X = seethrough:apply_template({string, "<h2><e:attr name=\"align\"><span e:replace=\"alignment\"/></e:attr></h2>"},
                [{alignment, "center"}]),
    "<h2 align=\"center\"/>" = stringify(X).

test_condition() ->
    X = seethrough:apply_template({string, "<div e:condition=\"error\">Boom!</div>"}, [{error, false}]),
    "" = stringify(X),
    X1 = seethrough:apply_template({string, "<div e:condition=\"error\">Boom!</div>"}, []),
    "" = stringify(X1),
    X2 = seethrough:apply_template({string, "<div e:condition=\"error\">Boom!</div>"}, [{error, true}]),
    "<div>Boom!</div>" = stringify(X2).

test_repeat() ->
    S = "<select><option e:repeat=\"member\">" ++
        "<span e:replace=\"name\"/>" ++
        "<e:attr name=\"value\" value=\"name\"/>" ++
        "<e:attr e:condition=\"current\" name=\"selected\">selected</e:attr>" ++
        "</option></select>",
    X = seethrough:apply_template({string, S}, [{member, [{name, "jim"}, {current, true}]},
                                                {member, [{name, "scotty"}]}]),
    "<select><option value=\"jim\" selected=\"selected\">jim</option>" ++
        "<option value=\"scotty\">scotty</option></select>" = stringify(X).

test_use_xml_as_values() ->
    S = "<span e:content=\"foreign\"/>",
    {El, _} = xmerl_scan:string("<i>hello</i>"),
    X = seethrough:apply_template({string, S}, [{foreign, El}]),
    "<span><i>hello</i></span>" = stringify(X).

test_retrieving_sub_element() ->
    S = "<ul><li e:repeat=\"members/member\"><span e:replace=\"name\"/></li></ul>",
    X = seethrough:apply_template({string, S}, [{members, [{member, [{name, "jim"}]},
                                                           {member, [{name, "spock"}]}]}]),
    "<ul><li>jim</li><li>spock</li></ul>" = stringify(X).

test_handle_element_with_custom_namespace() ->
    S = "<div xmlns:ex=\"http://hyperstruct.net\"><ex:url/></div>",
    X = seethrough:apply_template({string, S}, [], [{'http://hyperstruct.net', ?MODULE}]),
    "<div xmlns:ex=\"http://hyperstruct.net\">http://foo.bar.com</div>" = stringify(X).

test_handle_element_with_custom_namespace_as_child() ->
    S = "<div xmlns:ex=\"http://hyperstruct.net\">" ++
        "<a><e:attr name=\"href\"><ex:path for=\"config\"/></e:attr></a>" ++
        "</div>",
    X = seethrough:apply_template({string, S}, [], [{'http://hyperstruct.net', ?MODULE}]),
    "<div xmlns:ex=\"http://hyperstruct.net\">" ++
        "<a href=\"/app/prefix/config\"/></div>" = stringify(X).

test_handle_element_with_custom_namespace_and_content_1() ->
    S = "<div xmlns:ex=\"http://hyperstruct.net\"><ex:container>hello</ex:container></div>",
    X = seethrough:apply_template({string, S}, [], [{'http://hyperstruct.net', ?MODULE}]),
    "<div xmlns:ex=\"http://hyperstruct.net\"><div>hello</div></div>" = stringify(X).

test_handle_element_with_custom_namespace_and_content_2() ->
    S = "<div xmlns:ex=\"http://hyperstruct.net\">" ++
        "<ex:a><e:attr name=\"href\" value=\"bar\"/></ex:a>" ++
        "</div>",
    X = seethrough:apply_template({string, S}, [{bar, "hello"}],
                                  [{'http://hyperstruct.net', ?MODULE}]),
    "<div xmlns:ex=\"http://hyperstruct.net\"><ex:a href=\"hello\"/></div>" = stringify(X).

test_values_in_environment_can_be_functions() ->
    S = "<span e:replace=\"test\"/>",
    F = fun(_Env) -> #xmlText{value = "hello"} end,
    X = seethrough:apply_template({string, S}, [{foo, "bar"}, {test, F}]),
    "hello" = stringify(X).


%%%-------------------------------------------------------------------
%%% Helpers
%%%-------------------------------------------------------------------

stringify(Tree) ->
    lists:flatten(
      xmerl:export_simple(lists:flatten([Tree]),
                          xmerl_xml,
                          [#xmlAttribute{name = prolog,value = ""}])).


%%%-------------------------------------------------------------------
%%% Callbacks
%%%-------------------------------------------------------------------

compile(Node = #xmlElement{nsinfo = {_, "a"}, content = Content}, Attributes) ->
    Closures = seethrough:compile(Content),
    fun(Env) ->
            Results = seethrough:exec(Closures, Env),
            
            {ResultAttributes, ResultContents} =
                lists:partition(fun(N) -> is_record(N, xmlAttribute) end, Results),

            Node#xmlElement{attributes = Attributes ++ ResultAttributes,
                            content = ResultContents}
    end;

compile(#xmlElement{nsinfo = {_, "url"}}, _Attributes) ->
    fun(_Env) ->
            #xmlText{value = "http://foo.bar.com"}
    end;

compile(#xmlElement{nsinfo = {_, "container"}, content = Content},
        _Attributes) ->
    Closures = seethrough:compile(Content),
    fun(Env) ->
            #xmlElement{name = 'div',
                        content = seethrough:exec(Closures, Env)}
    end;
compile(#xmlElement{nsinfo = {_, "path"}}, Attributes) ->
    For = seethrough:get_attr_value(for, Attributes),
    fun(_Env) ->
            #xmlText{value = "/app/prefix/" ++ For}
    end.
    
