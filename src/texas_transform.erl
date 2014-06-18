-module(texas_transform).

-export([parse_transform/2]).

parse_transform(AST, Options) ->
  pt_helpers:transform(fun build/1, AST, Options).

build(PT_AST) ->
  Fields = pt_helpers:fields(PT_AST),
  TableName = pt_helpers:module_name(PT_AST),
  io:format("Generate module for table ~s~n", [TableName]),
  PT_AST1 = build_record(PT_AST, TableName, Fields),
  PT_AST2 = build_common_functions(PT_AST1, Fields),
  PT_AST3 = build_type_functions(PT_AST2, Fields),
  PT_AST4 = build_fields_function(PT_AST3, Fields),
  PT_AST5 = build_new_functions(PT_AST4, TableName),
  PT_AST6 = build_get_functions(PT_AST5, TableName, Fields),
  PT_AST7 = build_set_functions(PT_AST6, TableName, Fields),
  PT_AST8 = build_crud_functions(PT_AST7, TableName),
  PT_AST9 = build_tablepkid_function(PT_AST8, Fields),
  PT_ASTA = build_indexes_function(PT_AST9),
  PT_ASTB = build_refs_functions(PT_ASTA, TableName, Fields),
  PT_ASTB.

build_record(PT_AST, TableName, Fields) ->
  FieldsDef = lists:map(fun({Name, Options}) ->
          Type = case lists:keyfind(type, 1, Options) of
            {type, T} -> T;
            _ -> throw("No type found for field " ++ atom_to_list(Name) ++ " in table " ++ atom_to_list(TableName))
          end,
          {Name, Type}
      end, Fields),
  pt_helpers:add_record(PT_AST, TableName, FieldsDef).

build_common_functions(PT_AST, Fields) ->
  lists:foldl(fun({Option, Default}, PT_AST1) ->
        build_common_function(PT_AST1, Fields, Option, Default)
    end, PT_AST, [
      {autoincrement, {ok, false}},
      {not_null, {ok, false}},
      {unique, {ok, false}},
      {default, {none, null}},
      {len, {none, null}},
      {ref, {none, null}}
      ]).

build_type_functions(PT_AST, Fields) ->
  Clauses = lists:foldl(fun({FieldName, Options}, Clauses) ->
        Value = case lists:keyfind(type, 1, Options) of
          {type, A} -> A;
            _ -> throw("No type found for field " ++ atom_to_list(FieldName))
        end,
        Clauses ++ [pt_helpers:build_clause(
            pt_helpers:build_atom(FieldName),
            pt_helpers:build_value(Value)
          )]
    end, [], Fields),
  pt_helpers:add_function(PT_AST, export, type, Clauses).

build_fields_function(PT_AST, Fields) ->
  FieldsList = lists:map(fun({Name, _}) -> Name end, Fields),
  Clause = pt_helpers:build_clause([], pt_helpers:build_value(FieldsList)),
  pt_helpers:add_function(PT_AST, export, fields, Clause).

build_new_functions(PT_AST, TableName) ->
  Clause0 = pt_helpers:build_clause([], pt_helpers:build_record(TableName)),
  PT_AST0 = pt_helpers:add_function(PT_AST, export, new, Clause0),

  L = pt_helpers:build_var('L'),
  IsList = pt_helpers:build_call(is_list, L),
  R = pt_helpers:build_var('R'),
  F = pt_helpers:build_var('F'),
  V = pt_helpers:build_var('V'),
  Acc = pt_helpers:build_var('Acc'),
  FunBody = pt_helpers:build_call(TableName, F, [V, Acc]),
  FunClause = pt_helpers:build_clause(
      [pt_helpers:build_tuple({F, V}), Acc],
      FunBody
      ),
  Fun = pt_helpers:build_fun(FunClause),
  Body = [
      pt_helpers:build_match(R, pt_helpers:build_call(new, [])),
      pt_helpers:build_call(lists, foldl, [Fun, R, L])
      ],
  Clause1 = pt_helpers:build_clause([L], [IsList], Body),
  pt_helpers:add_function(PT_AST0, export, new, Clause1).

build_get_functions(PT_AST, TableName, Fields) ->
  lists:foldl(fun({FieldName, _}, PT_AST1) ->
        Clause = pt_helpers:build_clause(
            pt_helpers:build_var('R'),
            pt_helpers:build_get_record_field('R', TableName, FieldName)
            ),
        pt_helpers:add_function(PT_AST1, export, FieldName, Clause)
    end, PT_AST, Fields).
  
build_set_functions(PT_AST, TableName, Fields) ->
  lists:foldl(fun({FieldName, _}, PT_AST1) ->
        V = pt_helpers:build_var('V'),
        Type = pt_helpers:build_var('Type'),
        TypeCall = pt_helpers:build_call(type, [pt_helpers:build_atom(FieldName)]),
        TypeMatch = pt_helpers:build_match(Type, TypeCall),
        ToCall = pt_helpers:build_call(texas_type, to, [Type, V]),
        V1 = pt_helpers:build_var('V1'),
        ToV1 = pt_helpers:build_match(V1, ToCall),
        Field = pt_helpers:build_record_field(FieldName, V1),
        Record = pt_helpers:build_record('R', TableName, [Field]),
        R = pt_helpers:build_var('R'),
        Clause = [
            pt_helpers:build_clause(
              [pt_helpers:build_atom(undefined), R],
              [R]
              ),
            pt_helpers:build_clause(
              [V, R],
              [TypeMatch, ToV1, Record]
              )
            ],
        pt_helpers:add_function(PT_AST1, export, FieldName, Clause)
    end, PT_AST, Fields).

build_crud_functions(PT_AST, TableName) ->
  Table = pt_helpers:build_atom(TableName),
  Conn = pt_helpers:build_var('Conn'),
  Record = pt_helpers:build_var('Rec'),
  Arg1 = pt_helpers:build_var('Type'),
  Arg2 = pt_helpers:build_var('Clause'),
  FindClause = pt_helpers:build_clause(
      [Conn, Arg1, Arg2],
      pt_helpers:build_call(texas, find, [
          Conn,
          Table,
          Arg1,
          Arg2
          ])
      ),
  PT_AST1 = pt_helpers:add_function(PT_AST, export, find, FindClause),
  InsertClause = pt_helpers:build_clause(
      [Conn, Record],
      pt_helpers:build_call(texas, insert, [
          Conn,
          Table,
          Record
          ])
      ),
  PT_AST2 = pt_helpers:add_function(PT_AST1, export, insert, InsertClause),
  UpdateClause = pt_helpers:build_clause(
      [Conn, Arg1, Record],
      pt_helpers:build_call(texas, update, [
          Conn,
          Table,
          Arg1,
          Record
          ])
      ),
  PT_AST3 = pt_helpers:add_function(PT_AST2, export, update, UpdateClause),
  DeleteClause = pt_helpers:build_clause(
      [Conn, Record],
      pt_helpers:build_call(texas, delete, [
          Conn,
          Table,
          Record
          ])
      ),
  pt_helpers:add_function(PT_AST3, export, delete, DeleteClause).

build_tablepkid_function(PT_AST, Fields) ->
  IDS = lists:filter(fun({_, Desc}) ->
        lists:any(fun(E) -> E =:= {type, id} end, Desc) andalso
        lists:any(fun(E) -> E =:= {autoincrement, true} end, Desc)
    end, Fields),
  Resp = case length(IDS) of
    0 -> {none, null};
    1 -> 
      [{ID, _}] = IDS,
      {ok, ID};
    _ -> throw("Wrong table definition : multiple autoincrement ids")
  end,
  pt_helpers:add_function(
    PT_AST, export, table_pk_id, 
    pt_helpers:build_clause([], pt_helpers:build_tuple(Resp))).

build_indexes_function(PT_AST) ->
  pt_helpers:add_function(
    PT_AST, export, indexes, 
    pt_helpers:build_clause([], pt_helpers:build_list(
        pt_helpers:directive(PT_AST, index)))).

build_refs_functions(PT_AST, TableName, Fields) ->
  Refs = lists:filter(fun({_, Desc}) ->
        lists:any(fun({E, _}) -> E =:= ref end, Desc)
    end, Fields),
  case length(Refs) of
    0 -> PT_AST;
    _ -> lists:foldl(fun(Ref, PT_AST1) ->
            build_ref_function(PT_AST1, TableName, Ref)
        end, PT_AST, Refs)
  end.

%% - 

% Entity:ref(Conn)
build_ref_function(PT_AST, TableName, {FieldName, Desc}) ->
  case lists:keyfind(ref, 1, Desc) of
    false -> PT_AST;
    {ref, Table} -> 
      STable = atom_to_list(Table),
      case string:tokens(atom_to_list(FieldName), "_") of
        [STable|Rest] -> 
          Field = list_to_atom(string:join(Rest, "_")),
          % Table:find(Conn, all, [{where, "Field = :value", [{value, Self:FieldName()}]}]),
          Conn = pt_helpers:build_var('Conn'),
          Record = pt_helpers:build_var('Rec'),
          Ref = pt_helpers:build_var('Ref'),
          SetRefGuard = pt_helpers:build_guard(
              pt_helpers:build_op(
                '=:=', 
                pt_helpers:build_call(element, [pt_helpers:build_integer(1), Ref]), 
                pt_helpers:build_atom(Table))
              ),
          SetRefClause = pt_helpers:build_clause(
              [Ref, Record],
              SetRefGuard,
              pt_helpers:build_call(TableName, FieldName, [
                  pt_helpers:build_call(Table, Field, [Ref]),
                  Record
                  ])
              ),
          GetRefGuard = pt_helpers:build_guard(
              pt_helpers:build_op(
                '=:=', 
                pt_helpers:build_call(element, [pt_helpers:build_integer(1), Conn]), 
                pt_helpers:build_atom(texas))
              ),
          GetCallBody = pt_helpers:build_list([
                pt_helpers:build_tuple({
                    pt_helpers:build_atom(where),
                    pt_helpers:build_string(atom_to_list(Field) ++ " = :value"),
                    pt_helpers:build_list([
                        pt_helpers:build_tuple({
                            pt_helpers:build_atom(value),
                            pt_helpers:build_call(TableName, FieldName, [Record])
                            })
                        ])
                    })
                ]),
          GetRefClause = pt_helpers:build_clause(
              [Conn, Record],
              GetRefGuard,
              pt_helpers:build_call(texas, find, [
                  Conn,
                  pt_helpers:build_atom(Table),
                  pt_helpers:build_atom(all),
                  GetCallBody
                  ])
              ),
          pt_helpers:add_function(PT_AST, export, Table, [SetRefClause, GetRefClause]);
        _ -> PT_AST
      end
  end.

build_common_function(PT_AST, Fields, Option, Default) ->
  Clauses = build_common_clauses(Fields, Option, Default),
  pt_helpers:add_function(PT_AST, export, Option, Clauses).

build_common_clauses(Fields, Option, Default) ->
  lists:foldl(fun({FieldName, Options}, Clauses) ->
        Value = case lists:keyfind(Option, 1, Options) of
          {Option, A} -> {ok, A};
          _ -> Default
        end,
        Clauses ++ [pt_helpers:build_clause(
            pt_helpers:build_atom(FieldName),
            pt_helpers:build_tuple(Value)
          )]
    end, [], Fields).
