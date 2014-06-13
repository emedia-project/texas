-module(test).

-export([main/2]).

main(DBUser, Pass) ->
  texas:start(),
  texas_pgsql:start(),

  Conn = texas:connect("pgsql://" ++ DBUser ++ ":" ++ Pass ++ "@localhost:5432/texas"),
  T1 = texas:create_table(Conn, users),
  io:format("--> ~p~n", [T1]),
  T2 = texas:create_table(Conn, pipo),
  io:format("--> ~p~n", [T2]),

  % stateful modules.

  User = users:new([{name, "Greg"}, {mail, "glejeune@example.com"}]),
  User1 = User:title("Monsieur"),
  User2 = User1:insert(Conn),
  io:format("-> ~p~n", [User2]),
  
  Pipo = pipo:new([{key, "Hello"}, {value, "World"}]),
  Pipo1 = Pipo:insert(Conn),
  io:format("#> ~p~n", [Pipo1]),

  Pipo2 = pipo:new([{key, "Hola"}, {value, "Mundo"}]),
  Pipo3 = Pipo2:insert(Conn),
  io:format("#> ~p~n", [Pipo3]),


  UserA = users:new([{name, "Mu"}, {mail, "moger@example.com"}]),
  UserB = UserA:title("Madame"),
  UserC = UserB:insert(Conn),
  io:format("-> ~p~n", [UserC]),

  User3 = users:find(Conn, first, [{where, "name = :name AND mail = :mail", [{name, "Greg"}, {mail, "glejeune@example.com"}]}]),
  io:format("=> ~p~n", [User3]),

   User4 = User3:update(Conn, [{name, "Bob"}]),
   io:format("-> ~p~n", [User4]),

  User5 = users:find(Conn, first, [{where, "id = :id", [{id, User3:id()}]}]),
  io:format("=> ~p~n", [User5]),

  User6 = users:find(Conn, all, []),
  io:format("=> ~p~n", [User6]),

  % User3 = User,
  % User4 = User3:name("John"),
  % User3:insert(Conn),

  % User2:delete(Conn),
  X = User5:delete(Conn),
  io:format("-> ~p~n", [X]),

  User7 = users:find(Conn, all, []),
  io:format("=> ~p~n", [User7]),

  User8 = users:find(Conn, all, [{where, "name = :name", [{name, "%'; delete from users;"}]}]),
  io:format("=> ~p~n", [User8]),

  texas:close(Conn).
