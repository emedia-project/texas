-module(texas).

-export([start/0]).
-export([
  connect/1, 
  connect/2, 
  close/1, 
  find/4, 
  insert/3, 
  update/4, 
  delete/3, 
  create_table/2
]).

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

-type connection_string() :: string().
-type connection() :: #texas{}.
-type params() :: [any()].
-type table() :: atom().
-type func() :: atom().
-type data() :: any().
-type type() :: first | all.
-type clause() :: any(). % TODO

-spec start() -> any().
start() ->
  {ok, _} = application:ensure_all_started(lager).

% @doc
% Connect to the database.
%
% The <em>connection_string()</em> depends on the driver.
% @end
-spec connect(connection_string()) -> connection() | {error, any()}.
connect(URI) -> 
  connect(URI, []).
-spec connect(connection_string(), [tuple()]) -> connection() | {error, any()}.
connect(URI, Options) -> 
  case texas_uri:parse(URI) of
    {ok, {Scheme, User, Password, Server, Port, Path, _, _}} ->
      Module = list_to_atom("texas_" ++ Scheme),
      case erlang:apply(Module, connect, [
            User, Password, Server, Port, Path, Options
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
        _ -> {error, connection_failed}
      end;
    E -> E
  end.

-spec call(connection(), func()) -> any().
call(Conn, Function) ->
  call(Conn, Function, []).
-spec call(connection(), func(), params()) -> any().
call(Conn, Function, Params) ->
  erlang:apply(Conn#texas.module, Function, [Conn#texas.conn] ++ Params).

% @doc
% Create the given table (if not exists)
% @end
-spec create_table(connection(), table()) -> any().
create_table(Conn, Table) ->
  call(Conn, create_table, [Table]).

% @hidden
-spec insert(connection(), table(), data()) -> data().
insert(Conn, Table, Record) ->
  call(Conn, insert, [Table, Record]).

% @hidden
-spec find(connection(), table(), type(), clause()) -> data() | [data()].
find(Conn, Table, Type, Clause) ->
  call(Conn, select, [Table, Type, Clause]).

% @hidden
-spec update(connection(), table(), data(), data()) -> any().
update(Conn, Table, UpdateData, Record) ->
  call(Conn, update, [Table, Record, UpdateData]).

% @hidden
-spec delete(connection(), table(), data()) -> any().
delete(Conn, Table, Record) ->
  call(Conn, delete, [Table, Record]).

% @doc
% Close the connection to the database
% @end
-spec close(connection()) -> any().
close(Conn) ->
  call(Conn, close).
