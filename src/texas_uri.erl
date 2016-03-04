% @hidden
-module(texas_uri).

-export([parse/1]).

parse(URI) ->
  case re:run(URI, "([a-zA-Z]*):(.*)", [global, {capture, [1, 2], list}]) of
    {match, [[Scheme, Rest]|_]} -> parse(Scheme, Rest);
    _ -> {error, scheme_not_found}
  end.

parse(Scheme, "//" ++ Rest) ->
  case re:run(Rest, "([^/\\?]*)(/([^\\?#]*))?(\\?([^#]*))?(#?(.*))", [global, {capture, [1, 3, 5, 7], list}]) of
    {match, [[Authority, Path, Query, Fragment]|_]} -> 
      case parse_auth(Authority) of 
        {User, Pass, Server, Port} ->
          {ok, {Scheme, User, Pass, Server, Port, Path, parse_query(Query), Fragment}};
        E -> E
      end;
    _ -> {error, malformated_uri}
  end.

parse_query(Query) ->
  lists:map(fun(KV) ->
        [K|V] = string:tokens(KV, "="),
        {K, string:join(V, "=")}
    end, string:tokens(Query, "&")).

parse_auth([]) -> {"", "", "", ""};
parse_auth(Authority) ->
  [ServPort|Autz] = lists:reverse(string:tokens(Authority, "@")),
  LogPass = string:join(lists:reverse(Autz), "@"),
  case parse_serv(ServPort) of
    {error, E} -> {error, E};
    {Server, Port} ->
      {User, Password} = parse_user(LogPass),
      {User, Password, Server, Port}
  end.

parse_serv(ServPort) ->
  [Server|Port] = string:tokens(ServPort, ":"),
  if
    length(Port) > 1 -> {error, malformated_uri};
    length(Port) =:= 0 ->
      {Server, ""};
    true ->
      [Port1] = Port,
      {Server, Port1}
  end.

parse_user([]) -> {"", ""};
parse_user(LogPass) ->
  split_first(LogPass, ":").

split_first(String, Token) ->
  split_first(String, Token, {[], []}).
split_first([], _, R) -> R;
split_first([C|Rest], Token, {A, B}) ->
  case include(C, Token) of
    true -> {A, Rest};
    false -> split_first(Rest, Token, {A ++ [C], B})
  end.

include(X, List) ->
  lists:any(fun(E) -> E =:= X end, List).
