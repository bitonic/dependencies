-record(module_node, { name :: atom(),
                       to   :: [atom()],
                       from :: [atom()]
                     }).

-record(function_node, { mfa   :: mfa(),
                         to    :: [mfa()],
                         from  :: [mfa()]
                       }).

-type(graph_node() :: {#module_node{}, [#function_node{}]}).

-type(deps_graph() :: [graph_node()]).
