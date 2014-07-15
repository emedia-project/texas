-module(texas).

-export([start/0]).
-export([
  connect/0,
  connect/1, 
  connect/2, 
  close/1, 
  find/4, 
  insert/2, 
  update/3, 
  delete/3, 
  create_table/2,
  create_table/3,
  drop_table/2,
  to_keylist/2,
  connection/1,
  driver/1,
  create_habtm_table/3,
  get_habtm_data/4
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

-spec start() -> ok | {ok, connection()} | {error, any()}.
start() ->
  {ok, _} = application:ensure_all_started(lager),
  ok = application:ensure_started(texas),
  case get_value(autoconnect, false) of
    true -> case get_value(uri, undefined) of
        undefined -> ok;
        URI -> case connect(URI) of
            {error, E} -> {error, E};
            Conn -> {ok, Conn}
          end
      end;
    _ -> ok
  end.

% @doc
% Connect to the database.
% @end
-spec connect() -> connection() | {error, any()}.
connect() ->
  case get_value(uri, undefined) of
    undefined -> {error, autoconnect_faild};
    URI -> connect(URI)
  end.
-spec connect(connection_string()) -> connection() | {error, any()}.
connect(URI) -> 
  connect(URI, []).
-spec connect(connection_string(), [tuple()]) -> connection() | {error, any()}.
connect(URI, Options) -> 
  case texas_uri:parse(URI) of
    {ok, {Scheme, User, Password, Server, Port, Path, _, _}} ->
      Module = list_to_atom("texas_" ++ Scheme),
      case erlang:apply(Module, start, []) of
        ok ->
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
        _ -> {error, load_driver_faild}
      end;
    E -> E
  end.

% @hidden
-spec call(connection(), func()) -> any().
call(Conn, Function) ->
  call(Conn, Function, []).
% @hidden
-spec call(connection(), func(), params()) -> any().
call(Conn, Function, Params) ->
  erlang:apply(module(Conn), Function, [Conn] ++ Params).

% @doc
% Create the given table (if not exists)
% @end
-spec create_table(connection(), table()) -> any().
create_table(Conn, Table) ->
  lager:debug("== Create table ~p ==", [Table]),
  case lists:all(fun({_, Ref}) ->
          ok == create_habtm_table(Conn, Table, Ref)
      end, Table:'-habtm'()) of
    true -> call(Conn, create_table, [Table]);
    false -> error
  end.

% @doc
% Drop the given table (if exists)
% @end
-spec drop_table(connection(), table()) -> ok | error.
drop_table(Conn, Table) ->
  case call(Conn, drop_table, [Table]) of
    ok ->
      case texas_sql:defined_table(Table) of
        true ->
          case lists:all(fun({_, Ref}) ->
                  ok == drop_habtm_table(Conn, Table, Ref)
              end, Table:'-habtm'()) of
            true -> ok;
            false -> error
          end;
        false -> 
          ok
      end;
    E -> E
  end.

% @doc
% Create the given table (if not exists)
% @end
-spec create_table(connection(), table(), list()) -> any().
create_table(Conn, Table, Fields) ->
  lager:debug("== Create table ~p ==", [Table]),
  call(Conn, create_table, [Table, Fields]).

% @hidden
-spec find(connection(), table(), type(), clause()) -> data() | [data()].
find(Conn, Table, Type, Clause) ->
  call(Conn, select, [Table, Type, Clause]).

% @hidden
-spec insert(table(), data()) -> data().
insert(Table, Record) ->
  Result = call(Record, insert, [Table, Record]),
  lists:foreach(fun({Field, Ref}) ->
        _ = case Record:Field() of 
          undefined -> ok;
          Refs -> lists:foreach(fun(RefData) -> insert_habtm(Record, Table, Result:id(), Ref, RefData:id()) end, Refs)
        end
    end, Table:'-habtm'()),
  Result.
insert_habtm(Conn, From, FromID, To, ToID) ->
  Table = texas_sql:get_habtm_table(From, To),
  call(Conn, insert, [Table, [{habtm_rowid(From), FromID}, {habtm_rowid(To), ToID}]]).

% @hidden
-spec update(table(), data(), data()) -> any().
update(Table, UpdateData, Record) ->
  Data = Table:new(Record:'-conn'(), UpdateData),
  RealUpdateData = lists:filter(fun({_, Value}) ->
          Value =/= undefined
      end, Data:to_keylist()),
  Results = case RealUpdateData of
    [] -> [Record];
    _ -> call(Record, update, [Table, Record, RealUpdateData])
  end,
  _ = case Table:'-habtm'() of
    [] -> ok;
    _ -> 
      lists:foreach(fun(Result) ->
            lists:foreach(fun({Field, Ref}) ->
                  _ = case lists:keyfind(Field, 1, UpdateData) of 
                    {Field, Datas} -> update_habtm(Record, Table, Result:id(), Ref, Datas);
                    false -> ok
                  end
              end, Table:'-habtm'())
        end, Results)
  end,
  Results.
update_habtm(Conn, From, FromID, To, ToRecords) ->
  delete_habtm(Conn, From, FromID, To),
  lists:foreach(fun(ToRecord) ->
        insert_habtm(Conn, From, FromID, To, ToRecord:id())
    end, ToRecords).
delete_habtm(Conn, From, FromID, To) ->
  call(Conn, delete, [texas_sql:get_habtm_table(From, To), [{habtm_rowid(From), FromID}]]).

% @hidden
-spec delete(atom(), table(), data()) -> any().
delete(Type, Table, Record) ->
  lists:foreach(fun({Field, Relation, RefTable}) ->
        _ = case Relation of
          habtm -> ok;
          _ -> 
            lists:foreach(fun(Ref) ->
                  case Type of
                    recursive -> Ref:delete(recursive);
                    _ -> Ref:update([{RefTable:'-belongs_to_ref'(Table), null}])
                  end
              end, Record:Field())
        end
    end, Table:'-has'()),
  Result = call(Record, delete, [Table, Record]),
  lists:foreach(fun({_, Ref}) ->
        delete_habtm(Record, Table, Record:id(), Ref)
    end, Table:'-habtm'()),
  Result.

% @doc
% Close the connection to the database
% @end
-spec close(connection()) -> any().
close(Conn) ->
  call(Conn, close).

% @hidden
-spec connection(connection() | data()) -> any().
connection(Conn) when is_record(Conn, texas) -> 
  Conn#texas.conn;
connection(Conn) ->
  connection(Conn:'-conn'()).

-spec driver(connection() | data()) -> atom().
driver(Conn) when is_record(Conn, texas) -> 
  Conn#texas.driver;
driver(Conn) ->
  driver(Conn:'-conn'()).

% @hidden
-spec module(connection() | data()) -> any().
module(Conn) when is_record(Conn, texas) ->
  Conn#texas.module;
module(Conn) ->
  module(Conn:'-conn'()).

% @doc
% Return the record as keylist
% @end
-spec to_keylist(atom(), data()) -> list().
to_keylist(Table, Record) ->
  lists:map(fun(Field) ->
        {Field, Record:Field()}
    end, Table:'-fields'()).

% @hidden
-spec create_habtm_table(connection(), atom(), atom()) -> any().
create_habtm_table(Conn, Mod1, Mod2) ->
  JoinTableName = texas_sql:get_habtm_table(Mod1, Mod2),
  FieldA = {habtm_rowid(Mod1), [{type, id}]},
  FieldB = {habtm_rowid(Mod2), [{type, id}]},
  create_table(Conn, JoinTableName, [FieldA, FieldB]).

% @hidden
-spec drop_habtm_table(connection(), atom(), atom()) -> any().
drop_habtm_table(Conn, Mod1, Mod2) ->
  JoinTableName = texas_sql:get_habtm_table(Mod1, Mod2),
  drop_table(Conn, JoinTableName).

% @hidden
-spec get_habtm_data(connection(), atom(), atom(), any()) -> any().
get_habtm_data(Conn, From, To, FromID) ->
  case FromID of
    undefined -> undefined;
    _ -> 
      JoinTableName = texas_sql:get_habtm_table(From, To),
      JoinResults = call(Conn, select, [JoinTableName, all, [{where, [{habtm_rowid(From), FromID}]}]]),
      lists:foldl(fun(Join, Result) ->
            ToRow = habtm_rowid(To),
            case lists:keyfind(ToRow, 1, Join) of
              {ToRow, ToID} -> Result ++ [call(Conn, select, [To, first, [{where, [{id, texas_type:to(integer, ToID)}]}]])];
              false -> Result
            end
        end, [], JoinResults)
  end.

% @hidden
% Return the row ID for a given module
-spec habtm_rowid(atom()) -> atom().
habtm_rowid(Mod) ->
  list_to_atom(atom_to_list(Mod) ++ "_id").

get_value(Key, Default) ->
  case application:get_env(texas, Key) of
    {ok, Value} -> Value;
    _ -> Default
  end.
