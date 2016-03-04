% @hidden
-module(texas_type).
-compile([{parse_transform, lager_transform}]).

-export([to/2]).

% id
to(id, Value) -> to(integer, Value);
% integer
to(integer, Value) ->
  bucs:to_integer(Value);
% string
to(string, {{Y, M, D}, {H, MM, S}} = Date)
    when is_integer(Y),
    is_integer(M),
    is_integer(D),
    is_integer(H),
    is_integer(MM),
    is_integer(S) ->
  bucs:to_binary(bucdate:format("Y-m-d H:i:s", Date));
to(string, Value) ->
  bucs:to_binary(Value);
% text
to(text, Value) ->
  to(string, Value);
% float
to(float, Value) ->
  bucs:to_float(Value);
% date
to(date, Value) when is_list(Value) ->
  {Date, _} = bucdate:parse(Value),
  to(date, Date);
to(date, Value) when is_binary(Value) ->
  to(date, binary_to_list(Value));
to(date, {{Y, M, D}, {H, MM, S, _}}) ->
  to(date, {{Y, M, D}, {H, MM, S}});
to(date, {Date, _}) ->
  to(date, Date);
to(date, {Y, M, D} = Date)
    when is_integer(Y),
    is_integer(M),
    is_integer(D) ->
  bucs:to_binary(bucdate:format("Y-m-d", {Date, {0, 0, 0}}));
% time
to(time, Value) when is_list(Value) ->
  {_, Time} = bucdate:parse(Value),
  to(time, Time);
to(time, Value) when is_binary(Value) ->
  to(time, binary_to_list(Value));
to(time, {{Y, M, D}, {H, MM, S, _}}) ->
  to(time, {{Y, M, D}, {H, MM, S}});
to(time, {_, Time}) ->
  to(time, Time);
to(time, {H, MM, S} = Time)
    when is_integer(H),
    is_integer(MM),
    is_integer(S) ->
  bucs:to_binary(bucdate:format("H:i:s", {{0, 0, 0}, Time}));
% datetime
to(datetime, Value) when is_list(Value) ->
  to(datetime, bucdate:parse(Value));
to(datetime, Value) when is_binary(Value) ->
  to(datetime, binary_to_list(Value));
to(datetime, {{Y, M, D}, {H, MM, S, _}}) ->
  to(datetime, {{Y, M, D}, {H, MM, S}});
to(datetime, {{Y, M, D}, {H, MM, S}} = Date)
    when is_integer(Y),
    is_integer(M),
    is_integer(D),
    is_integer(H),
    is_integer(MM),
    is_integer(S) ->
  bucs:to_binary(bucdate:format("Y-m-d H:i:s", Date));
% Error
to(Type, Value) ->
  lager:error("Can't convert ~p to ~p", [Value, Type]),
  exit(io_lib:format("Can't convert ~p to ~p", [Value, Type])).
