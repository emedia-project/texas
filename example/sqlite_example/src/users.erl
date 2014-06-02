-module(users).
-compile([{parse_transform, texas_transform}]).


% Options :
%   {type, Type} where Type = integer|string|float|date|time|datetime (required)
%   {autoincrement, Auto} where Auto = true|false (default: false)
%   {len, Size} where Size is a number or a tuple (default: null)
%   {not_null, NN} where NN = true|false (default: false)
%   {unique, Uniq} where Uniq = true|false (default: false)
%   {default, Def} where Def = item() (default: null)
%   {ref, Table} where Table = atom() (default: null)

-field({id,         [{type, id},      {autoincrement, true}                 ]}).
-field({name,       [{type, string},  {len, 255},           {not_null, true}]}).
-field({mail,       [{type, string},  {unique, true}                        ]}).
-field({title,      [{type, string},  {not_null, true},     {default, "M."} ]}).
-field({address_id, [{type, id},      {ref, address}                        ]}).


