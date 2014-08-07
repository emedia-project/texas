-module(pipo).
-compile([{parse_transform, texas_transform}]).

-field({key, [{type, string}]}).
-field({value, [{type, string}]}).
-timestamps(true).
