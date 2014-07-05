-module(texas_rebar).
-export([
  'db-create'/2,
  'db-drop'/2,
  'db-seed'/2
  ]).

-define(CONSOLE(Str, Args), io:format(Str++"~n", Args)).

% {plugins, [texas_rebar]}

'db-create'(Config, _) ->
  case run(Config) of
    true -> create(Config);
    false -> ok
  end.

'db-drop'(Config, _) ->
  case run(Config) of
    true -> drop(Config);
    false -> ok
  end.

'db-seed'(_Config, _) ->
  ?CONSOLE("TODO", []).

run(Config) ->
  BaseDir = rebar_config:get_xconf(Config, base_dir),
  Cwd = rebar_utils:get_cwd(),
  if 
    BaseDir =:= Cwd ->
      Ebin = filename:join([BaseDir, "ebin"]),
      code:add_patha(Ebin),
      true;
    true -> false
  end.

create(Config) ->
  case connect(Config) of
    {error, E} -> ?CONSOLE(E, []);
    {ok, Conn, Tables} ->
      lists:foreach(fun(Table) ->
            case code:load_file(Table) of
              {error, E} -> 
                ?CONSOLE("!! Faild to load module ~p: ~p", [Table, E]);
              _ ->
                case texas:create_table(Conn, Table) of
                  error -> 
                    ?CONSOLE("=== Faild to create table ~p ===", [Table]);
                  _ ->
                    ?CONSOLE("=== Table ~p created ===", [Table])
                end
            end
        end, Tables),
      texas:close(Conn)
  end.

drop(Config) ->
  case connect(Config) of
    {error, E} -> ?CONSOLE(E, []);
    {ok, Conn, Tables} ->
      lists:foreach(fun(Table) ->
            case code:load_file(Table) of
              {error, E} -> 
                ?CONSOLE("!! Faild to load module ~p: ~p", [Table, E]);
              _ ->
                case texas:drop_table(Conn, Table) of
                  error -> 
                    ?CONSOLE("=== Faild to drop table ~p ===", [Table]);
                  _ -> 
                    ?CONSOLE("=== Table ~p dropped ===", [Table])
                end
            end
        end, Tables),
      texas:close(Conn)
  end.

connect(Config) ->
  ConfigFile = rebar_config:get_global(Config, texas, "config/sys.config"),
  case file:consult(ConfigFile) of
    {ok, [Data|_]} ->
      case lists:keyfind(texas, 1, Data) of
        {texas, DBInfos} -> 
          ?CONSOLE("** Read configuration", []),
          case lists:keyfind(uri, 1, DBInfos) of
            {uri, URI} -> 
              case lists:keyfind(tables, 1, DBInfos) of
                {tables, Tables} ->
                  ?CONSOLE("=== Connection to ~p ===", [URI]),
                  texas:start(),
                  case texas:connect(URI) of
                    {error, _} -> 
                      {error, "!! Can't connect to the database"};
                    Conn ->
                      {ok, Conn, Tables}
                  end;
                _ ->
                  {error, "!! No tables"}
              end;
            _ -> 
              {error, "!! Missing database connection string"}
          end;
        _ ->
          {error, "!! Database informations not found"}
      end;
    {error, _} -> 
      {error, "!! Can't read configuration file"}
  end.

