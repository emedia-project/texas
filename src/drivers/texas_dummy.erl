-module(texas_dummy).

-export([connect/5, exec/2, close/1]).
-export([create_table/2]).
-export([insert/3, select/4, update/4, delete/3]).

connect(_User, _Password, _Server, _Port, _Database) ->
  {ok, dummy_connect}.

exec(_SQL, _Conn) ->
  ok.

close(_Conn) ->
  ok.

create_table(_Conn, _Table) ->
  ok. % {error, Reason}

insert(_Conn, _Table, _Record) ->
  _Record.

select(_Conn, _Table, _Type, _Clauses) ->
  _Table:new().

update(_Conn, _Table, _Record, _UpdateData) ->
  _Table:new().

delete(_Conn, _Table, _Record) ->
  ok.
