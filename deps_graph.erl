-module(deps_graph).

-include("deps.hrl").

-export([start/1]).
-export([application_graph/1]).

-spec module_node(atom()) -> #module_node{}.
% -spec function_node(atom()) -> #function_node{}.
-spec dependency_graph(gb_tree()) -> dependency_graph().
-spec split_modules([mfa()]) -> gb_tree().
-spec application_graph(string()) -> dependency_graph().

start([Filepath]) -> application_graph(Filepath).

split_modules(Functions) ->
    lists:foldl(
      fun({M, F, A}, Modules) ->
              L = case gb_trees:lookup(M, Modules) of
                      {value, L0} -> L0;
                      none        -> []
                  end,
              gb_trees:enter(M, [{M, F, A} | L], Modules)
      end, gb_trees:empty(), Functions).

module_node(Module) ->
    QTo = format("ME | ~p", [Module]),
    {ok, ToE} = xref:q(s, QTo),
    To = lists:map(fun snd/1, ToE),

    QFrom = format("ME || ~p", [Module]),
    {ok, FromE} = xref:q(s, QFrom),
    From = lists:map(fun fst/1, FromE),

    #module_node{ name = Module,
                  to   = To,
                  from = From}.

function_node(Function) ->
    FunName = function_name(Function),
    {ok, ToE} = xref:q(s, format("E | ~s", [FunName])),
    To = lists:map(fun snd/1, ToE),

    {ok, FromE} = xref:q(s, format("E || ~s", [FunName])),
    From = lists:map(fun fst/1, FromE),

    #function_node{ mfa  = Function,
                    to   = To,
                    from = From
                  }.

dependency_graph(Modules) ->
    lists:map(
      fun({M, MFuns}) ->
              Nodes = lists:map(fun function_node/1, MFuns),
              {module_node(M), Nodes}
      end, gb_trees:to_list(Modules)).

application_graph(Filepath) ->
    xref:start(s),
    xref:add_application(s, Filepath),
    {ok, Functions} = xref:q(s, "F"),
    Modules = split_modules(Functions),
    Graph = dependency_graph(Modules),
    xref:stop(s),
    Graph.

function_name({M,F,Ar}) ->
    format("~p:~p/~p", [M,F,Ar]).

format(Fmt, Args) ->
    lists:flatten(io_lib:format(Fmt, Args)).

fst({X, _}) -> X.
snd({_, X}) -> X.
