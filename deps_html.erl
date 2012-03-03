-module(deps_html).

-export([modules/1, modules/2]).

-include("deps.hrl").
-import(deps_misc, [format/2, intercalate/2, term_to_string/1]).

%% -type(html() :: string()).

%% -spec modules(deps_graph()) -> html().
%% -spec modules(deps_graph(), html()) -> 'ok'.
%% -spec modules_list(deps_graph()) -> html().
%% -spec module_link(atom()) -> html().
%% -spec module(#module_node{}, deps_graph()) -> html().
%% -spec module_heading(atom()) -> html().
%% -spec deps_table({list(atom()), list(atom())}) -> html().
%% -spec anchor(string(), string()) -> html().
%% -spec link(string(), string()) -> html().
%% -spec html_page(string(), html()) -> html().
%% -spec table(string(), list(string()), list(list(html()))) -> html().
%% -spec ul(list(html())) -> html().

-define(PAGE_STYLE,
        "td {"
        "  vertical-align: top;"
        "}"
        ".module {"
        "  width: 80%;"
        "  padding: 0;"
        "}"
        ".module td {"
        "  border: 1px solid black;"
        "  padding: 0;"
        "  width: 50%;"
        "}"
        ".deps {"
        "  width: 100%;"
        "}"
        ".deps td {"
        "  border: 1px solid black;"
        "  padding: 0.5em;"
        "  width: 50%;"
        "}"
        "ul {"
        "  list-style: none;"
        "  padding: 0;"
        "  margin: 0;"
        "}"
        ).

modules(Graph) ->
    Modules = lists:flatmap(fun({MN, _FNs}) -> module(MN, Graph) end, Graph),
    Body = [modules_list(Graph) | Modules],
    Html = html_page(?PAGE_STYLE, Body),
    xmerl:export_simple([Html], xmerl_xml).

modules(Graph, Filepath) ->
    deps_misc:write_to_file(modules(Graph), Filepath).

%% ----

modules_list(Graph) ->
    ul(lists:map(
         fun ({#module_node { name = Name }, _FNs}) ->
                 [module_link(Name)]
         end, Graph)).

module_link(Module) ->
    S = term_to_string(Module),
    link(S, [S]).

module(#module_node { name = Name,
                      to = To,
                      from = From
                    }, Graph) ->
    Partition = fun(Ms) -> lists:partition(
                             fun (M) ->
                                     case deps_graph:lookup(M, Graph) of
                                         none       -> true;
                                         {value, _} -> false
                                     end
                             end, Ms)
                end,
    ToSep = Partition(To),
    FromSep = Partition(From),
    [{hr, []},
     module_heading(Name),
     table([{class, "module"}],
           ["From", "To"],
           [[[deps_table(FromSep)], [deps_table(ToSep)]]])].

module_heading(Module) ->
    S = term_to_string(Module),
    anchor(S, [{h2, [S]}]).

deps_table({Ext, Int}) ->
    HExt = ul(lists:map(single(fun deps_misc:term_to_string/1), Ext)),
    HInt = ul(lists:map(single(fun module_link/1), Int)),
    table([{class, "deps"}],
          ["External", "Internal"],
          [[[HExt], [HInt]]]).

anchor(Anchor, Body) -> {a, [{name, Anchor}], Body}.

link(Anchor, Body) -> {a, [{href, "#" ++ Anchor}], Body}.

html_page(Style, Body) ->
    {html, [{head, [{style, [Style]}]}, {body, Body}]}.

table(Attrs, Titles, Rows) ->
    HTitles = {tr, Attrs, lists:map(fun (T) -> {th, [T]} end, Titles)},
    HRows = lists:map(fun (R) ->
                              {tr, lists:map(fun (C) -> {td, C} end, R)}
                      end, Rows),
    {table, Attrs, [HTitles | HRows]}.

ul(Lis) -> ul([], Lis).

ul(Attrs, Lis) ->
    {ul, Attrs, lists:map(fun (Li) -> {li, Li} end, Lis)}.

single(F) ->
    fun (X) -> [F(X)] end.
