-module(texas_type).

-export([to/2]).

% id
to(id, Value) -> to(integer, Value);
% integer
to(integer, Value) when is_integer(Value) ->
  Value;
to(integer, Value) when is_float(Value) ->
  trunc(Value);
to(integer, Value) when is_list(Value) ->
  list_to_integer(Value);
to(integer, Value) when is_binary(Value) ->
  binary_to_integer(Value);
to(integer, Value) when is_atom(Value) ->
  list_to_integer(atom_to_list(Value));
% string
to(string, Value) when is_integer(Value) ->
  integer_to_list(Value);
to(string, Value) when is_float(Value) ->
  integer_to_list(trunc(Value));
to(string, Value) when is_list(Value) ->
  Value;
to(string, Value) when is_binary(Value) ->
  binary_to_list(Value);
to(string, Value) when is_atom(Value) ->
  atom_to_list(Value);
to(string, {{Y, M, D}, {H, MM, S}} = Date) 
    when is_integer(Y), 
    is_integer(M), 
    is_integer(D),
    is_integer(H),
    is_integer(MM),
    is_integer(S) ->
  edate:format("Y-m-d H:i:s", Date);
% text
to(text, Value) when is_integer(Value) ->
  integer_to_list(Value);
to(text, Value) when is_float(Value) ->
  integer_to_list(trunc(Value));
to(text, Value) when is_list(Value) ->
  Value;
to(text, Value) when is_binary(Value) ->
  binary_to_list(Value);
to(text, Value) when is_atom(Value) ->
  atom_to_list(Value);
to(string, {{Y, M, D}, {H, MM, S, _}}) ->
  to(string, {{Y, M, D}, {H, MM, S}});
to(string, {{Y, M, D}, {H, MM, S}} = Date) 
    when is_integer(Y), 
    is_integer(M), 
    is_integer(D),
    is_integer(H),
    is_integer(MM),
    is_integer(S) ->
  edate:format("Y-m-d H:i:s", Date);
% float
to(float, Value) when is_integer(Value) ->
  float(Value);
to(float, Value) when is_float(Value) ->
  Value;
to(float, Value) when is_list(Value) ->
  list_to_float(Value);
to(float, Value) when is_binary(Value) ->
  list_to_float(binary_to_list(Value));
to(float, Value) when is_atom(Value) ->
  list_to_float(atom_to_list(Value));
% date
to(date, Value) when is_list(Value) ->
  {Date, _} = edate:parse(Value),
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
  edate:format("Y-m-d", {Date, {0, 0, 0}});
% time
to(time, Value) when is_list(Value) ->
  {_, Time} = edate:parse(Value),
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
  edate:format("H:i:s", {{0, 0, 0}, Time});
% datetime
to(datetime, Value) when is_list(Value) ->
  to(datetime, edate:parse(Value));
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
  edate:format("Y-m-d H:i:s", Date);
% Error
to(Type, Value) -> 
  lager:error("Can't convert ~p to ~p", [Value, Type]),
  exit(io_lib:format("Can't convert ~p to ~p", [Value, Type])).
