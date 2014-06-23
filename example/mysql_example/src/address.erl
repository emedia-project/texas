-module(address).
-compile([{parse_transform, texas_transform}]).

-field({id,     [{type, id},     {autoincrement, true}]}).
-field({street, [{type, string}, {len, 255}           ]}).
-field({city,   [{type, string}, {len, 255}           ]}).
-field({user,   [               {has_many, users}    ]}).

