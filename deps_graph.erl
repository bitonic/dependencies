-module(deps_graph).

-include("deps.hrl").
-import(deps_misc, [format/2]).

-export([start/1]).
-export([new/1, lookup/2]).

-spec module_node(atom()) -> #module_node{}.
-spec function_node(mfa()) -> #function_node{}.
-spec deps_graph(gb_tree()) -> deps_graph().
-spec split_modules([mfa()]) -> gb_tree().
-spec new(string()) -> deps_graph().
-spec lookup(atom(), deps_graph()) -> ('none' | {'ok', graph_node()}).

start([Filepath]) -> new(Filepath).

split_modules(Functions) ->
    lists:foldl(
      fun({M, _, _} = F, Modules) ->
              Fs = case gb_trees:lookup(M, Modules) of
                       {value, Fs0} -> Fs0;
                       none         -> []
                   end,
              gb_trees:enter(M, [F | Fs], Modules)
      end, gb_trees:empty(), Functions).

module_node(Module) ->
    F = fun (M) -> M =/= Module end,
    {ok, ToE} = xref:q(s, format("ME | ~w", [Module])),
    To = lists:filter(F, lists:map(fun deps_misc:snd/1, ToE)),
    {ok, FromE} = xref:q(s, format("ME || ~w", [Module])),
    From = lists:filter(F, lists:map(fun deps_misc:fst/1, FromE)),
    #module_node{ name = Module,
                  to   = To,
                  from = From
                }.

function_node(Function) ->
    FunName = deps_misc:function_name(Function),
    {ok, ToE} = xref:q(s, format("E | ~s", [FunName])),
    To = lists:map(fun deps_misc:snd/1, ToE),
    {ok, FromE} = xref:q(s, format("E || ~s", [FunName])),
    From = lists:map(fun deps_misc:fst/1, FromE),
    #function_node{ mfa  = Function,
                    to   = To,
                    from = From
                  }.

deps_graph(Modules) ->
    lists:map(fun({M, MFuns}) ->
                      Nodes = lists:map(fun function_node/1, MFuns),
                      {module_node(M), Nodes}
              end, gb_trees:to_list(Modules)).

new(Filepath) ->
    xref:start(s),
    xref:add_application(s, Filepath),
    {ok, Functions} = xref:q(s, "F"),
    Modules = split_modules(Functions),
    Graph = deps_graph(Modules),
    xref:stop(s),
    Graph.

lookup(_M, []) ->
    none;
lookup(M, [{#module_node { name = M1 }, _FN} = N | Graph]) ->
    case M =:= M1 of
        true  -> {value, N};
        false -> lookup(M, Graph)
    end.
