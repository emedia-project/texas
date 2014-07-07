-module(texas_transform).

-export([parse_transform/2]).

parse_transform(AST, Options) ->
  pt_helpers:transform(fun build/1, AST, Options).

build(PT_AST) ->
  Fields = update_fields(pt_helpers:fields(PT_AST)),
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
  PT_ASTB = build_belongs_to_functions(PT_ASTA, TableName, Fields),
  PT_ASTC = build_has_functions(PT_ASTB, TableName, Fields),
  PT_ASTD = build_to_keylist_function(PT_ASTC, TableName),
  PT_ASTE = build_conn_function(PT_ASTD, TableName),
  PT_ASTE.

update_fields(Fields) ->
  lists:map(fun({Field, Options}) ->
        case lists:keyfind(belongs_to, 1, Options) of
          false ->
            {Field, Options};
          {belongs_to, _} ->
            {list_to_atom(atom_to_list(Field) ++ "_id"),
             merge_keylists(1, Options, [{type, id}])}
        end
    end, Fields).

build_record(PT_AST, TableName, Fields) ->
  FieldsDef = lists:foldl(fun({Name, Options}, Acc) ->
          Acc ++ case lists:keyfind(type, 1, Options) of
            {type, Type} -> [{Name, Type}];
            _ -> case lists:keyfind(habtm, 1, Options) of
                {habtm, Ref} -> [{list_to_atom(atom_to_list(Ref) ++ "_habtm"), term}];
                _ -> []
              end
          end
      end, [], Fields) ++ [{'__texas_conn', term}],
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
      {belongs_to, {none, null}}
      ]).

build_type_functions(PT_AST, Fields) ->
  Clauses = lists:foldl(fun({FieldName, Options}, Clauses) ->
        Clauses ++ case lists:keyfind(type, 1, Options) of
            {type, Value} -> 
              [pt_helpers:build_clause(
                  pt_helpers:build_atom(FieldName),
                  pt_helpers:build_value(Value)
                  )];
            _ -> []
        end
    end, [], Fields),
  pt_helpers:add_function(PT_AST, export, '-type', Clauses).

build_fields_function(PT_AST, Fields) ->
  FieldsList = lists:foldl(fun({Name, Options}, Acc) ->
          Acc ++ case lists:keyfind(type, 1, Options) of
            {type, _} -> [Name];
            _ -> []
          end
      end, [], Fields),
  Clause = pt_helpers:build_clause([], pt_helpers:build_value(FieldsList)),
  pt_helpers:add_function(PT_AST, export, '-fields', Clause).

build_new_functions(PT_AST, TableName) ->
  Conn = pt_helpers:build_var('Conn'),
  Clause0 = pt_helpers:build_clause(
      [Conn], 
      pt_helpers:build_record(
        TableName,
        [pt_helpers:build_record_field('__texas_conn', Conn)]
        )),
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
      pt_helpers:build_match(R, pt_helpers:build_call(new, [Conn])),
      pt_helpers:build_call(lists, foldl, [Fun, R, L])
      ],
  Clause1 = pt_helpers:build_clause([Conn, L], [IsList], Body),
  pt_helpers:add_function(PT_AST0, export, new, Clause1).

build_get_functions(PT_AST, TableName, Fields) ->
  lists:foldl(fun({FieldName, Options}, PT_AST1) ->
        case lists:keyfind(type, 1, Options) of
          {type, _} ->
            Clause = pt_helpers:build_clause(
                pt_helpers:build_var('R'),
                pt_helpers:build_get_record_field('R', TableName, FieldName)
                ),
            pt_helpers:add_function(PT_AST1, export, FieldName, Clause);
          _ -> PT_AST1
        end
    end, PT_AST, Fields).
  
build_set_functions(PT_AST, TableName, Fields) ->
  lists:foldl(fun({FieldName, Options}, PT_AST1) ->
        case lists:keyfind(type, 1, Options) of
          {type, _} ->
            V = pt_helpers:build_var('V'),
            Type = pt_helpers:build_var('Type'),
            TypeCall = pt_helpers:build_call('-type', [pt_helpers:build_atom(FieldName)]),
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
                  [pt_helpers:build_atom(null), R],
                  pt_helpers:build_record('R', TableName, [
                      pt_helpers:build_record_field(FieldName, pt_helpers:build_atom(null))])
                  ),
                pt_helpers:build_clause(
                  [V, R],
                  [TypeMatch, ToV1, Record]
                  )
                ],
            pt_helpers:add_function(PT_AST1, export, FieldName, Clause);
          _ -> PT_AST1
        end
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
      [Record],
      pt_helpers:build_call(texas, insert, [
          Table,
          Record
          ])
      ),
  PT_AST2 = pt_helpers:add_function(PT_AST1, export, insert, InsertClause),
  UpdateClause = pt_helpers:build_clause(
      [Arg1, Record],
      pt_helpers:build_call(texas, update, [
          Table,
          Arg1,
          Record
          ])
      ),
  PT_AST3 = pt_helpers:add_function(PT_AST2, export, update, UpdateClause),
  DeleteClauseDefault = pt_helpers:build_clause(
      [Record],
      pt_helpers:build_call(texas, delete, [
          pt_helpers:build_atom(non_recursive),
          Table,
          Record
          ])
      ),
  PT_AST4 = pt_helpers:add_function(PT_AST3, export, delete, DeleteClauseDefault),
  DeleteClause = pt_helpers:build_clause(
      [Arg1, Record],
      pt_helpers:build_call(texas, delete, [
          Arg1,
          Table,
          Record
          ])
      ),
  pt_helpers:add_function(PT_AST4, export, delete, DeleteClause).

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
    PT_AST, export, '-table_pk_id', 
    pt_helpers:build_clause([], pt_helpers:build_tuple(Resp))).

build_indexes_function(PT_AST) ->
  pt_helpers:add_function(
    PT_AST, export, '-indexes', 
    pt_helpers:build_clause([], pt_helpers:build_list(
        pt_helpers:directive(PT_AST, index)))).

build_belongs_to_functions(PT_AST, TableName, Fields) ->
  Refs = lists:filter(fun({_, Desc}) ->
        lists:any(fun({E, _}) -> E =:= belongs_to end, Desc)
    end, Fields),
  case length(Refs) of
    0 -> PT_AST;
    _ -> lists:foldl(fun(Ref, PT_AST1) ->
            build_belongs_to_function(PT_AST1, TableName, Ref)
        end, PT_AST, Refs)
  end.

build_has_functions(PT_AST, TableName, Fields) ->
  {PT_AST2, HABTM1, Has1} = lists:foldl(fun({Field, Options}, {PT_AST1, HABTM, Has}) ->
          case lists:keyfind(has_one, 1, Options) of
            {has_one, Ref} -> 
              {build_has_one_function(Field, Ref, TableName, PT_AST1), HABTM, Has ++ [{Field, has_one, Ref}]};
            _ -> case lists:keyfind(has_many, 1, Options) of
                {has_many, Ref} -> 
                  {build_has_many_function(Field, Ref, TableName, PT_AST1), HABTM, Has ++ [{Field, has_many, Ref}]};
                _ -> case lists:keyfind(habtm, 1, Options) of
                    {habtm, Ref} ->
                      {build_habtm_function(Field, Ref, TableName, PT_AST1), HABTM ++ [{Field, Ref}], Has ++ [{Field, habtm, Ref}]};
                    _ -> {PT_AST1, HABTM, Has}
                  end
              end
          end
      end, {PT_AST, [], []}, Fields),
  PT_AST3 = build_habtm_list_function(HABTM1, PT_AST2),
  build_has_list_function(Has1, PT_AST3).

build_to_keylist_function(PT_AST, TableName) ->
  Record = pt_helpers:build_var('Rec'),
  ToKLClause = pt_helpers:build_clause(
      [Record],
      pt_helpers:build_call(texas, to_keylist, [
          pt_helpers:build_atom(TableName),
          Record
          ])
      ),
  pt_helpers:add_function(PT_AST, export, to_keylist, ToKLClause).

build_conn_function(PT_AST, TableName) ->
  Record = pt_helpers:build_var('Rec'),
  ConnClause = pt_helpers:build_clause(
      [Record],
      pt_helpers:build_get_record_field(Record, TableName, '__texas_conn')),
  pt_helpers:add_function(PT_AST, export, '-conn', ConnClause).

%% - 

build_has_one_function(Field, Ref, TableName, PT_AST) ->
  build_has_one_or_many_function(Field, Ref, TableName, PT_AST, first).

build_has_many_function(Field, Ref, TableName, PT_AST) ->
  build_has_one_or_many_function(Field, Ref, TableName, PT_AST, all).

build_has_one_or_many_function(Field, Ref, TableName, PT_AST, FindType) ->
  Record = pt_helpers:build_var('Rec'),
  HasOneFindClause = pt_helpers:build_list([
        pt_helpers:build_tuple({
            pt_helpers:build_atom(where),
            pt_helpers:build_list([
                pt_helpers:build_tuple({
                    pt_helpers:build_call(Ref, '-ref_col', [pt_helpers:build_atom(TableName)]),
                    pt_helpers:build_call(TableName, id, [Record])
                    })
                ])
            })
        ]),
  HasOneClause = pt_helpers:build_clause(
      [Record],
      pt_helpers:build_call(texas, find, [
          pt_helpers:build_get_record_field(Record, TableName, '__texas_conn'),
          pt_helpers:build_atom(Ref),
          pt_helpers:build_atom(FindType),
          HasOneFindClause
          ])
      ),
  pt_helpers:add_function(PT_AST, export, Field, HasOneClause).

build_habtm_function(Field, Ref, TableName, PT_AST) ->
  Record = pt_helpers:build_var('Record'),
  GetClause = pt_helpers:build_clause(
      Record,
      pt_helpers:build_case(
        pt_helpers:build_get_record_field('Record', TableName, list_to_atom(atom_to_list(Ref) ++ "_habtm")),
        [pt_helpers:build_clause(
            pt_helpers:build_atom(undefined), 
            pt_helpers:build_call(
              texas,
              get_habtm_data,
              [pt_helpers:build_get_record_field(Record, TableName, '__texas_conn'),
               pt_helpers:build_atom(TableName), 
               pt_helpers:build_atom(Ref), 
               pt_helpers:build_get_record_field('Record', TableName, id)])),
         pt_helpers:build_clause(
            pt_helpers:build_var('X'),
            pt_helpers:build_var('X'))]
        )
      ),
  PT_AST1 = pt_helpers:add_function(PT_AST, export, Field, GetClause),
  SetClause = pt_helpers:build_clause(
      [pt_helpers:build_var('Data'), Record],
      pt_helpers:build_record('Record', TableName, [
          pt_helpers:build_record_field(list_to_atom(atom_to_list(Ref) ++ "_habtm"), 
                                        pt_helpers:build_var('Data'))])),
  pt_helpers:add_function(PT_AST1, export, Field, SetClause).

build_habtm_list_function(HABTM, PT_AST) ->
  HabtmListClause = pt_helpers:build_clause(
      [],
      pt_helpers:build_list(HABTM)),
  pt_helpers:add_function(PT_AST, export, '-habtm', HabtmListClause).

build_has_list_function(Has, PT_AST) ->
  HasListClause = pt_helpers:build_clause(
      [],
      pt_helpers:build_list(Has)),
  pt_helpers:add_function(PT_AST, export, '-has', HasListClause).

% Entity:belongs_to(Conn)
build_belongs_to_function(PT_AST, TableName, {FieldName, Desc}) ->
  case lists:keyfind(belongs_to, 1, Desc) of
    false -> PT_AST;
    {belongs_to, Table} -> 
      ["id"|Rest] = lists:reverse(string:tokens(atom_to_list(FieldName), "_")),
      RealField = list_to_atom(string:join(lists:reverse(Rest), "_")),
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
              pt_helpers:build_call(Table, id, [Ref]),
              Record
              ])
          ),
      SetRefClauseUndefined = pt_helpers:build_clause(
          [pt_helpers:build_atom(undefined), Record],
          pt_helpers:build_call(TableName, FieldName, [
              pt_helpers:build_atom(undefined),
              Record
              ])
          ),
      SetRefClauseNull = pt_helpers:build_clause(
          [pt_helpers:build_atom(null), Record],
          pt_helpers:build_call(TableName, FieldName, [
              pt_helpers:build_atom(null),
              Record
              ])
          ),
      PT_AST1 = pt_helpers:add_function(PT_AST, export, RealField, [SetRefClause, SetRefClauseUndefined, SetRefClauseNull]),

      GetCallBody = pt_helpers:build_list([
            pt_helpers:build_tuple({
                pt_helpers:build_atom(where),
                pt_helpers:build_list([
                    pt_helpers:build_tuple({
                        pt_helpers:build_atom(id),
                        pt_helpers:build_var('ID')
                        })
                    ])
                })
            ]),
      GetCall = pt_helpers:build_case(
        pt_helpers:build_call(TableName, FieldName, [Record]),
        [pt_helpers:build_clause(
            pt_helpers:build_atom(undefined), 
            pt_helpers:build_atom(undefined)),
         pt_helpers:build_clause(
            pt_helpers:build_var('ID'),
            pt_helpers:build_call(texas, find, [
                pt_helpers:build_get_record_field(Record, TableName, '__texas_conn'),
                pt_helpers:build_atom(Table),
                pt_helpers:build_atom(first),
                GetCallBody
                ])
            )]
        ),
      GetRefClause = pt_helpers:build_clause([Record], GetCall),
      PT_AST2 = pt_helpers:add_function(PT_AST1, export, RealField, GetRefClause),
      RefColClause = pt_helpers:build_clause(
          [pt_helpers:build_atom(Table)],
          pt_helpers:build_atom(FieldName)),
      pt_helpers:add_function(PT_AST2, export, '-ref_col', RefColClause)
  end.

build_common_function(PT_AST, Fields, Option, Default) ->
  Clauses = build_common_clauses(Fields, Option, Default),
  pt_helpers:add_function(PT_AST, export, list_to_atom("-" ++ atom_to_list(Option)), Clauses).

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

%% @doc
%% Merge the two keylists.
%%
%% Example:
%% <pre>
%% Args = [{a, 1}, {b, 2}],
%% Default = [{b, 3}, {c, 4}],
%% elists:merge_keylists(1, Args, Default),
%%   #=> [{c, 4}, {a, 1}, {b, 2}]
%% </pre>
%% @end
merge_keylists(_, [], TupleList2) ->
  TupleList2;
merge_keylists(N, [Tuple|Rest], TupleList2) when 
    is_integer(N), is_list(TupleList2), is_tuple(Tuple), is_list(Rest) ->
  Key = element(N, Tuple),
  TupleList3 = case lists:keysearch(Key, N, TupleList2) of
    {value, _} -> lists:keydelete(Key, N, TupleList2);
    false -> TupleList2
  end,
  merge_keylists(N, Rest, TupleList3 ++ [Tuple]).
