-module(deps_dot).

-export([modules/1, modules/2]).

-include("deps.hrl").
-import(deps_misc, [format/2]).

-spec modules(deps_graph()) -> string().
-spec modules(deps_graph(), string()) -> 'ok'.

modules1([], _Labels) ->
    "";
modules1([{#module_node { name = Name,
                          to   = To,
                          from = _From
                        }, _} | Rest], Labels) ->
    Label = gb_trees:get(Name, Labels),
    format("\t~B [label=\"~p\"]~n", [Label, Name]) ++
        lists:flatmap(
          fun(M) ->
                  %% We don't graph external modules
                  case gb_trees:lookup(M, Labels) of
                      none ->
                          "";
                      {value, Label1} ->
                          format("\t~B -> ~B~n", [Label, Label1])
                  end
          end, To) ++
        modules1(Rest, Labels).

modules(Graph) ->
    Names = lists:map(
              fun ({#module_node {name = Name}, _}) -> Name end,
              Graph),
    "digraph callraph {~n" ++ modules1(Graph, label(Names)) ++ "}".

modules(Graph, Filepath) ->
    deps_misc:write_to_file(modules(Graph), Filepath).

label(Graph) ->
    {_, Labels} = lists:foldl(
                    fun(El, {C, M}) ->
                            {C+1, gb_trees:insert(El, C, M)}
                    end, {1, gb_trees:empty()}, Graph),
    Labels.
