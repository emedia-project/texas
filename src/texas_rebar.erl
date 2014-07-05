-module(texas_rebar).
-export([
  'db-create'/2
  ]).

-define(CONSOLE(Str, Args), io:format(Str++"~n", Args)).

% {plugins, [texas_rebar]}

'db-create'(Config, _) ->
  BaseDir = rebar_config:get_xconf(Config, base_dir),
  Cwd = rebar_utils:get_cwd(),
  if 
    BaseDir =:= Cwd ->
      Ebin = filename:join([BaseDir, "ebin"]),
      code:add_patha(Ebin),
      migrate(Config);
    true -> ok
  end.

migrate(Config) ->
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
                    {error, _} -> ?CONSOLE("!! Can't connect to the database", []);
                    Conn ->
                      lists:foreach(fun(Table) ->
                            case code:load_file(Table) of
                              {error, E} -> 
                                ?CONSOLE("!! Faild to load module ~p: ~p", [Table, E]);
                              _ ->
                                case texas:create_table(Conn, Table) of
                                  {error, E} -> 
                                    ?CONSOLE("=== Table ~p error: ~p ===", [Table, E]);
                                  _ ->
                                    ?CONSOLE("=== Table ~p created ===", [Table])
                                end
                            end
                        end, Tables),
                      texas:close(Conn)
                  end;
                _ ->
                  ?CONSOLE("!! No tables", [])
              end;
            _ -> 
              ?CONSOLE("!! Missing database connection string", [])
          end;
        _ ->
          ?CONSOLE("!! Database informations not found", [])
      end;
    {error, _} -> ?CONSOLE("!! Can't read configuration file", [])
  end.

