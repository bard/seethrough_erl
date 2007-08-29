-module(dynvar).
-export([with/2, fetch/1, test/0]).

with(Bindings, Action) ->
    lists:foreach(fun({Name, Value}) -> put(Name, Value) end, Bindings),
    try apply(Action, []) of
        X -> X
    after
        lists:foreach(fun({Name, _Value}) -> erase(Name) end, Bindings)
    end.

fetch(VarName) ->
    get(VarName).

test() ->
    io:format("before with: ~p~n", [get(foo)]),
    Res = with([{foo, 1}, {bar, 2}],
         fun() ->
                 io:format("during with: ~p~n", [dynvar:fetch(abc)]),
                 2
         end),
    io:format("Res: ~p~n", [Res]),
    io:format("after with: ~p~n", [get(foo)]).

