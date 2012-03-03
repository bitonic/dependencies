-module(deps_dot).

-export([modules/2, modules/3]).

-include("deps.hrl").
-import(deps_misc, [format/2, shuffle/1]).

-spec modules(deps_graph(), [atom()]) -> string().
-spec modules(deps_graph(), [atom()], string()) -> 'ok'.

modules(Graph, Exclude) ->
    "digraph callraph {~n"
        "concentrate=true~n" ++
        lists:append(modules1(Graph, Exclude, Graph)) ++ "}".

modules(Graph, Exclude, Filepath) ->
    deps_misc:write_to_file(modules(Graph, Exclude), Filepath).

%% ----

modules1([], _E, _G) ->
    "";
modules1([{#module_node {name = Name, to = To}, _} | Rest], Exclude, Graph) ->
    case lists:member(Name, Exclude) of
        true ->
            modules1(Rest, Exclude, Graph);
        false ->
            lists:map(
              fun(M) ->
                      %% We don't graph external modules
                      case lists:member(M, Exclude) orelse
                  deps_graph:lookup(M, Graph) =:= none of
                          true  -> "";
                          false -> format("\t~p -> ~p~n", [Name, M])
                      end
              end, To) ++ modules1(Rest, Exclude, Graph)
    end.
