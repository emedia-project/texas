# Texas

A simple ORM for [paris](https://github.com/emedia-project/paris) -- But not only!

## Create 

```erlang
-module(person).
-compile([{parse_transform, texas_transform}]).

-field({id,         [{type, id},      {autoincrement, true}                 ]}).
-field({name,       [{type, string},  {not_null, true}                      ]}).
-field({mail,       [{type, string},  {unique, true}                        ]}).
-field({title,      [{type, string},  {not_null, true},     {default, "M."} ]}).
-field({address_id, [{type, integer}, {ref, address}                        ]}).
```

## Drivers

* sqlite : https://github.com/emedia-project/texas_sqlite
* pgsql : TODO
* mysql : TODO

## Usage

```erlang
application:start(texas),
application:start(texas_sqlite), % Start driver

% Initialize connection
Conn = texas:connect("sqlite:///sample.db"),

% Create table (if not exist)
ok = texas:create_table(Conn, person),

% Create a new Person and insert it
Person = person:new([{name, "Greg"}, {mail, "gregoire.lejeune"}]),
Person1 = Person:insert(Conn),
io:format("person created with ID #~p~n", [Person1:id()]),

% Find
Person2 = person:find(Conn, first, [{where, "name = :name", [{name, "Greg"}]}]),

% Update
Person3 = Person2:update(Conn, [{name, "Bob"]]),

% Delete
ok = Person3:delete(Conn).
```
