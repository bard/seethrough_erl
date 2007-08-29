-module(env).
-export([lookup/2, lookup/3, sample/0]).
-compile([export_all]).

lookup(Path, List) ->
    case lookup(first, Path, List) of
        undefined -> undefined;
        {_VarName, Value} -> {value, Value}
    end.

lookup_real(Path, List) ->
    lookup(first, Path, List).

lookup(first, Name, List) when is_atom(Name) ->
    lookup(first, [Name], List);
lookup(first, Path, List) ->
    %% This looks up all results even though it only uses first one.
    %% There is room for optimizatin in lookup1/2.
    case lookup1(normalize(Path), List) of
        [] -> undefined;
        [First|_Rest] -> First
    end;
lookup(all, Name, List) when is_atom(Name) ->
    lookup(all, [Name], List);
lookup(all, Path, List) ->
    lookup1(normalize(Path), List).

lookup1([NodeName], List) ->
    proplists:lookup_all(NodeName, List);
lookup1([NodeName|RestPath], List) ->
    lookup1(RestPath, 
            lists:flatten([Children || {_NodeName, Children} <- lookup1([NodeName], List)])).

%%% Removes '..' and '.' items, and changes path accordingly.  Path is
%%% assumed to be absolute, so ['..'] will normalize to []
%%% (i.e. root).

%% normalize([Step|Rest]) when is_list(Step) ->
%%     normalize([list_to_atom(Step)|Rest]);
normalize([Step|Rest]) when is_atom(Step) ->
    normalize([Step|Rest], []);
normalize(Step) when is_atom(Step) ->
    normalize([Step], []);
normalize(Step) when is_list(Step) ->
    normalize(string_to_path(Step), []).

normalize(['..'|Rest], [_PoppedStep|NormRest]) ->
    normalize(Rest, NormRest);
normalize(['.'|Rest], NormalizedPath) ->
    normalize(Rest, NormalizedPath);
normalize([Step|Rest], NormalizedPath) ->
    normalize(Rest, [Step|NormalizedPath]);
normalize([], NormalizedPath) ->
    lists:reverse(NormalizedPath).



sample() ->
    [{ships,
      [{ship, [{name, "Enterprise"},
               {members, [[{name, "Kirk"}],
                          [{name, "Spock"}]]}]},
       {ship, [{name, "Defiant"},
               {members, [[{name, "Sulu"}]]}]}
      ]},
     {foo, "bar"}].



string_to_path(SPath) ->
    [list_to_atom(Step) || Step <- string:tokens(SPath, "/")].

