-module(deps_html).

-export([modules/1, modules/2]).

-include("deps.hrl").
-import(deps_misc, [format/2, intercalate/2]).

-spec module(#module_node{}, deps_graph()) -> string().
-spec modules(deps_graph()) -> string().
-spec modules(deps_graph(), string()) -> 'ok'.
-spec link(string(), string(), string()) -> string().
-spec html_page(string(), string()) -> string().
-spec table(string(), list(), list()) -> string().

-define(PAGE_STYLE,
        "td {"
        "  vertical-align: top;"
        "}"
        ".module {"
        "  width: 60em;"
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
        "  padding: 0.5em 0;"
        "  width: 50%;"
        "}"
        ).


deps_table({Ext, Int}) ->
    HExt = ul(lists:map(fun deps_misc:term_to_string/1, Ext)),
    HInt = ul(lists:map(fun deps_misc:term_to_string/1, Int)),
    table("deps",
          ["External", "Internal"],
          [[HExt, HInt]]).

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
    format("<h2>~p</h2><br/>", [Name]) ++
        table("module",
              ["From", "To"],
              [[deps_table(FromSep), deps_table(ToSep)]]).

modules(Graph) ->
    html_page(?PAGE_STYLE,
              lists:flatmap(fun({MN, _FNs}) -> module(MN, Graph) end, Graph)).

modules(Graph, Filepath) ->
    deps_misc:write_to_file(modules(Graph), Filepath).

link(Href, Anchor, Body) ->
    format("<a href=\"~s#~s\">~s</a>", [Href, Anchor, Body]).

html_page(Style, Body) ->
    T = "<html>"
        "<head>"
        "<style>~s</style>"
        "</head>"
        "<body>~s</body>"
        "</html>",
    format(T, [Style, Body]).

table(Class, Titles, Rows) ->
    HTitles = "<tr>" ++ lists:flatmap(fun (_) -> "<th>~s</th>" end, Titles) ++ "</tr>",
    HRows =
        lists:flatmap(
          fun (R) -> "<tr>" ++
                         lists:flatmap(fun (_) -> "<td>~s</td>" end, R) ++
                         "</tr>"
          end, Rows),
    format("<table class=\"~s\">", [Class]) ++
        format(HTitles ++ HRows, Titles ++ lists:concat(Rows)) ++
        "</table>".

ul(Els) ->
    "<ul>" ++
        lists:flatmap(fun (El) -> "<li>" ++ El ++ "</li>" end, Els)
        ++ "</ul>".
