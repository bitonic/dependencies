-module(deps_misc).
-export([format/2, fst/1, snd/1, function_name/1, write_to_file/2,
         intercalate/2, term_to_string/1]).

format(Fmt, Args) ->
    lists:flatten(io_lib:format(Fmt, Args)).

fst({X, _}) -> X.
snd({_, X}) -> X.

function_name({M,F,Ar}) ->
    deps_misc:format("~p:~p/~p", [M,F,Ar]).

write_to_file(String, Filepath) ->
    {ok, File} = file:open(Filepath, [write]),
    io:format(File, String, []),
    ok = file:close(File).

intercalate(_X, []) ->
    [];
intercalate(_X, [Y]) ->
    [Y];
intercalate(X, [Y | XS]) ->
    Y ++ X ++ intercalate(X, XS).

term_to_string(T) -> format("~w", [T]).
