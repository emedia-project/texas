-module(address).
-compile([{parse_transform, texas_transform}]).

-field({id,     [{type, id},    {autoincrement, true}]}).
-field({street, [{type, string}                      ]}).
-field({city,   [{type, string}                      ]}).
-field({user,   [               {has_many, users}    ]}).

