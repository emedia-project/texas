-module(texas_sql).

-export([record_to_where_clause/2]).

record_to_where_clause(Table, Record) ->
  {Strs, Tpls} = lists:foldl(fun(Field, {AccStr, AccTpl}) ->
          case Record:Field() of
            undefined -> {AccStr, AccTpl};
            Value ->
              {AccStr ++ [io_lib:format("~p = :~p", [Field, Field])], AccTpl ++ [{Field, Value}]}
          end
      end, {[], []}, Table:fields()),
  {where, string:join(Strs, " AND "), Tpls}.

