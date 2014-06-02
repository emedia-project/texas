-module(texas_type).

-export([to/2]).

% integer
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
to(date, _) ->
  exit(todo); % TODO
% time
to(time, _) ->
  exit(todo); % TODO
% datetime
to(datetime, _) ->
  exit(todo); % TODO
% Error
to(Type, Value) -> 
  exit(io_lib:format("Can't convert ~p to ~p", [Value, Type])).
