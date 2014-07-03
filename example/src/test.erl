-module(test).

-export([
  sqlite/0,
  mysql/2,
  pgsql/2,
  main/1
]).

sqlite() ->
  texas_sqlite:start(),
  main("sqlite:///sample.db").

mysql(DBUser, Pass) ->
  texas_mysql:start(),
  main("mysql://" ++ DBUser ++ ":" ++ Pass ++ "@localhost:3306/texas").

pgsql(DBUser, Pass) ->
  texas_pgsql:start(),
  main("pgsql://" ++ DBUser ++ ":" ++ Pass ++ "@localhost:5432/texas").

main(URI) ->
  texas:start(),

  Conn = texas:connect(URI),
  io:format("Conn : ~p~n", [Conn]),
  T1 = texas:create_table(Conn, users),
  io:format("T1 : ~p~n", [T1]),
  texas:create_table(Conn, address),
  texas:create_table(Conn, pipo),
  texas:create_table(Conn, device),

  % stateful modules.

  User = users:new(Conn, [{name, "Greg"}, {mail, "glejeune@example.com"}]),
  User1 = User:title("Monsieur"),
  User2 = User1:insert(),
  io:format("User2 -> ~p~n", [User2:to_keylist()]),
  
  Pipo = pipo:new(Conn, [{key, "Hello"}, {value, "World"}]),
  Pipo1 = Pipo:insert(),
  io:format("Pipo1 -> ~p~n", [Pipo1:to_keylist()]),

  Pipo2 = pipo:new(Conn, [{key, "Hola"}, {value, "Mundo"}]),
  Pipo3 = Pipo2:insert(),
  io:format("Pipo3 -> ~p~n", [Pipo3:to_keylist()]),

  UserA = users:new(Conn, [{name, "Mu"}, {mail, "moger@example.com"}]),
  UserB = UserA:title("Madame"),
  UserC = UserB:insert(),
  io:format("UserC -> ~p~n", [UserC:to_keylist()]),

  User3 = users:find(Conn, first, [{where, [{name, "=", "Greg"}, {mail, "glejeune@example.com"}]}]),
  io:format("User3 -> ~p~n", [User3:to_keylist()]),

  User4 = User3:update([{name, "Bob"}]),
  io:format("User4 -> ~p~n", [lists:map(fun(U) -> U:to_keylist() end, User4)]),

  User5 = users:find(Conn, first, [{where, [{id, User3:id()}]}]),
  io:format("User5 -> ~p~n", [User5:to_keylist()]),

  User6 = users:find(Conn, all, []),
  io:format("User6 -> ~p~n", [lists:map(fun(U) -> U:to_keylist() end, User6)]),

  X = User5:delete(),
  io:format("X -> ~p~n", [X]),

  User7 = users:find(Conn, all, []),
  io:format("User7 -> ~p~n", [lists:map(fun(U) -> U:to_keylist() end, User7)]),

  User8 = users:find(Conn, first, [{where, [{name, "like", "%'; delete from users;"}]}]),
  io:format("User8 -> ~p~n", [User8]),

  Address = address:new(Conn, [{street, "1 avenue des Champs Elysees"}, {city, "Paris"}]),
  Address1 = Address:insert(),
  io:format("Address1 -> ~p~n", [Address1:to_keylist()]),

  User9 = users:new(Conn, [{name, "John"}, {mail, "john@doe.com"}, {live_at, Address1}]),
  User10 = User9:insert(),
  io:format("User10 -> ~p~n", [User10:to_keylist()]),
  Address2 = User10:live_at(),
  io:format("Address2 -> ~p~n", [Address2:to_keylist()]),
  Address3 = address:new(Conn, [{street, "2 rue de la paix"}, {city, "Paris"}]),
  Address4 = Address3:insert(),
  io:format("Address4 -> ~p~n", [Address4:to_keylist()]),
  User11 = User10:update([{live_at, Address4}]),
  io:format("User11 -> ~p~n", [lists:map(fun(U) -> U:to_keylist() end, User11)]),
  User12 = Address4:user(),
  io:format("User12 -> ~p~n", [lists:map(fun(U) -> U:to_keylist() end, User12)]),

  Device1 = device:new(Conn, [{type, "iPhone"}]),
  Device2 = Device1:insert(),
  Device3 = device:new(Conn, [{type, "iPad"}]),
  Device4 = Device3:insert(),
  User13 = users:new(Conn, [{name, "Tim"}, {mail, "tim@apple.com"}, {devices, [Device2, Device4]}]),
  User14 = User13:insert(),

  DevicesForUser14 = User14:devices(),
  io:format("DevicesForUser14 -> ~p~n", [lists:map(fun(U) -> U:to_keylist() end, DevicesForUser14)]),
  UsersForDevice4 = Device4:users(),
  io:format("UsersForDevice4 -> ~p~n", [lists:map(fun(U) -> U:to_keylist() end, UsersForDevice4)]),

  Device5 = Device4:update([{users, UsersForDevice4 ++ User12}]),
  UsersForDevice5 = Device5:users(),
  io:format("UsersForDevice5 -> ~p~n", [lists:map(fun(U) -> U:to_keylist() end, UsersForDevice5)]),

  User14:delete(),

  texas:close(Conn).
