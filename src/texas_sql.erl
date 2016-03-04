% @hidden
-module(texas_sql).

-export([
  clause/2,
  insert_clause/2,
  where_clause/2,
  set_clause/2,
  sql_string/2,
  sql_field/2,
  defined_table/1,
  get_option/2,
  get_habtm_table/2
]).

clause(What, Clauses) when is_atom(What), is_list(Clauses) ->
  case lists:keyfind(What, 1, Clauses) of
    {What, WhatClause} -> WhatClause;
    false -> []
  end.

insert_clause(Data, Driver) when is_list(Data) ->
  insert_clause(Data, Driver, "", "", "");
insert_clause(Record, Driver) when is_tuple(Record)->
  insert_clause(record_to_keylist(Record), Driver).
insert_clause([], _, _, KeyList, ValueList) ->
  " (" ++ KeyList ++ ") VALUES (" ++ ValueList ++ ")";
insert_clause([{Key, Value}|Rest], Driver, Separator, KeyList, ValueList) ->
  insert_clause(
    Rest, Driver, ", ",
    io_lib:format("~s~s~s", [
        KeyList, Separator,
        sql_field(Key, Driver)]),
    io_lib:format("~s~s~s", [
        ValueList, Separator,
        sql_string(Value, Driver)])).

where_clause([], _) -> "";
where_clause(Data, Driver) when is_list(Data) ->
  kv_clause(Data, Driver, fun(K, Op, V, D) ->
        case V of
          null -> D:where_null(Op, K);
          _ -> io_lib:format("~s ~s ~s", [texas_sql:sql_field(K, D), Op, texas_sql:sql_string(V, D)])
        end
    end, "", " AND ", " WHERE ");
where_clause(Record, Driver) when is_tuple(Record) ->
  where_clause(record_to_keylist(Record), Driver).

set_clause(Data, Driver) when is_list(Data) ->
  kv_clause(Data, Driver, fun(K, Op, V, D) ->
        case V of
          null -> D:set_null(K);
          _ -> io_lib:format("~s ~s ~s", [texas_sql:sql_field(K, D), Op, texas_sql:sql_string(V, D)])
        end
    end , "", ", ", " SET ");
set_clause(Record, Driver) when is_tuple(Record) ->
  set_clause(record_to_keylist(Record), Driver).

sql_string(Value, Driver) when is_binary(Value); is_bitstring(Value) ->
  sql_string(bucs:to_string(Value), Driver);
sql_string(Value, Driver) when is_list(Value) ->
  quote(Value, Driver:string_separator(), Driver:string_quote());
sql_string(Value, _) ->
  io_lib:format("~p", [Value]).

sql_field(Value, Driver) ->
  case Driver:field_separator() of
    none -> io_lib:format("~p", [Value]);
    SepChar -> io_lib:format("~c~p~c", [SepChar, Value, SepChar])
  end.

defined_table(Module) ->
  case is_atom(Module) of
    true ->
      try Module:module_info() of
        _InfoList ->
          true
      catch
        _:_ ->
          false
      end;
    false ->
      false
  end.

get_option(type, Options) ->
  case lists:keyfind(type, 1, Options) of
    {type, Type} -> Type;
    false -> throw("Not type defined")
  end;
get_option(autoincrement, Options) ->
  case lists:keyfind(autoincrement, 1, Options) of
    {autoincrement, Value} -> {ok, Value};
    _ -> {ok, false}
  end;
get_option(not_null, Options) ->
  case lists:keyfind(not_null, 1, Options) of
    {not_null, Value} -> {ok, Value};
    _ -> {ok, false}
  end;
get_option(unique, Options) ->
  case lists:keyfind(unique, 1, Options) of
    {unique, Value} -> {ok, Value};
    _ -> {ok, false}
  end;
get_option(default, Options) ->
  case lists:keyfind(default, 1, Options) of
    {default, Value} -> {ok, Value};
    _ -> {none, null}
  end;
get_option(len, Options) ->
  case lists:keyfind(len, 1, Options) of
    {len, Value} -> {ok, Value};
    _ -> {none, null}
  end.

get_habtm_table(Mod1, Mod2) ->
  list_to_atom(
    string:join(
      lists:sort([
          atom_to_list(Mod1),
          atom_to_list(Mod2)
          ]), "_to_")).

% priv

kv_clause([], _, _, _, _, Result) -> Result;
kv_clause([Entry|Rest], Driver, SetOpFun, CurrentSeparator, NextSeparator, Result) ->
  case Entry of
    {Key, Value} -> kv_clause([{Key, "=", Value}|Rest], Driver, SetOpFun, CurrentSeparator, NextSeparator, Result);
    {Key, Operator, Value} ->
      kv_clause(
        Rest, Driver, SetOpFun, NextSeparator, NextSeparator,
        io_lib:format("~s~s~s", [
            Result, CurrentSeparator,
            SetOpFun(Key, Operator, Value, Driver)]));
    _ -> error({error, invalide_clause_data})
  end.

record_to_keylist(Record) when is_tuple(Record) ->
  case defined_table(element(1, Record)) of
    true -> lists:filter(fun({_, Value}) ->
            Value =/= undefined
        end, Record:to_keylist());
    false -> error({error, where_clause_error})
  end.

quote(Str, SepChar, QuoteChar) ->
    quote_(Str, SepChar, QuoteChar, [SepChar]).
quote_([], SepChar, _, R) ->
    R++[SepChar];
quote_([SepChar|Rest], SepChar, QuoteChar, R) ->
    quote_(Rest, SepChar, QuoteChar, R ++ [QuoteChar, SepChar]);
quote_([QuoteChar|Rest], SepChar, QuoteChar, R) ->
    quote_(Rest, SepChar, QuoteChar, R ++ [QuoteChar, QuoteChar]);
quote_([X|Rest], SepChar, QuoteChar, R) ->
    quote_(Rest, SepChar, QuoteChar, R ++ [X]).

