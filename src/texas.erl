-module(texas).

-export([connect/1, close/1, find/4, insert/3, update/4, delete/3, create_table/2]).

-record(texas, {
    driver,
    module,
    user,
    password,
    server,
    port,
    database,
    connection_string,
    conn
    }).

connect(URI) -> 
  case texas_uri:parse(URI) of
    {ok, {Scheme, User, Password, Server, Port, Path, _, _}} ->
      Module = list_to_atom("texas_" ++ Scheme),
      case erlang:apply(Module, connect, [
            User, Password, Server, Port, Path
            ]) of
        {ok, Conn} -> 
          #texas{
            driver = Scheme,
            module = Module,
            user = User,
            password = Password,
            server = Server,
            port = Port,
            database = Path,
            connection_string = URI,
            conn = Conn
            };
        E -> E
      end;
    E -> E
  end.

call(Conn, Function) ->
  call(Conn, Function, []).
call(Conn, Function, Params) ->
  erlang:apply(Conn#texas.module, Function, [Conn#texas.conn] ++ Params).

create_table(Conn, Table) ->
  call(Conn, create_table, [Table]).

insert(Conn, Table, Record) ->
  call(Conn, insert, [Table, Record]).

find(Conn, Table, Type, Clause) ->
  call(Conn, select, [Table, Type, Clause]).

update(Conn, Table, UpdateData, Record) ->
  call(Conn, update, [Table, Record, UpdateData]).

delete(Conn, Table, Record) ->
  call(Conn, delete, [Table, Record]).

close(Conn) ->
  call(Conn, close).
