-module(device).
-compile([{parse_transform, texas_transform}]).

-field({id,     [{type, id},    {autoincrement, true}]}).
-field({type,   [{type, string}                      ]}).
-field({users,  [               {habtm, users}       ]}).

