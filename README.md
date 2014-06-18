# Texas

A simple ORM for [paris](https://github.com/emedia-project/paris)

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

and

```erlang
-module(address).
-compile([{parse_transform, texas_transform}]).

-field({id,         [{type, id},      {autoincrement, true}]}).
-field({street,     [{type, string}                        ]}).
-field({city,       [{type, string}                        ]}).
-field({zipcode,    [{type, string}                        ]}).
```

## Drivers

* sqlite : https://github.com/emedia-project/texas_sqlite
* PostgreSQL : https://github.com/emedia-project/texas_pgsql
* MySQL : https://github.com/emedia-project/texas_mysql

## Usage

```erlang
texas:start(),
texas_sqlite:start(), % Start driver

% Initialize connection
Conn = texas:connect("sqlite:///sample.db"),

% Create tables (if not exist)
ok = texas:create_table(Conn, person),
ok = texas:create_table(Conn, address),

% Create a new address
Address = address:new([{street, "21 jump street"}, {city, "New Orleans"}, {zip, "70112"}]).
Address1 = Address:insert(Conn).

% Create a new Person and insert it
Person = person:new([{name, "Greg"}, {mail, "gregoire.lejeune"}]),
Person1 = Person:address(Address1), 
Person2 = Person:insert(Conn),
io:format("person created with ID #~p~n", [Person2:id()]),

% Find
Person3 = person:find(Conn, first, [{where, "name = :name", [{name, "Greg"}]}]),
Address2 = Person3:address(Conn),
% or
Address_id = Person3:address_id(),

% Update
Person4 = Person3:update(Conn, [{name, "Bob"]]),

% Delete
ok = Person4:delete(Conn).
```
