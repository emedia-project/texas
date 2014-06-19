-module(texas_sql).

-export([
  record_to_where_clause/2, 
  record_to_where_clause/3,
  to_sql_string/2,
  to_sql_string/3,
  to_sql_field/2
  ]).

record_to_where_clause(Table, Record) ->
  record_to_where_clause(Table, Record, "").
record_to_where_clause(Table, Record, FieldQuote) ->
  {Strs, Tpls} = lists:foldl(fun(Field, {AccStr, AccTpl}) ->
          case Record:Field() of
            undefined -> {AccStr, AccTpl};
            Value ->
              {AccStr ++ [io_lib:format("~s~p~s = :~p", [FieldQuote, Field, FieldQuote, Field])], AccTpl ++ [{Field, Value}]}
          end
      end, {[], []}, Table:fields()),
  {where, string:join(Strs, " AND "), Tpls}.

to_sql_string(Value, SepChar) ->
  to_sql_string(Value, SepChar, $\\). 
to_sql_string(Value, SepChar, QuoteChar) when is_list(Value) ->
  quote(Value, SepChar, QuoteChar);
to_sql_string(Value, _, _) ->
  io_lib:format("~p", [Value]).

to_sql_field(Value, SepChar) ->
  case SepChar of
    none -> io_lib:format("~p", [Value]);
    _ -> io_lib:format("~c~p~c", [SepChar, Value, SepChar])
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
