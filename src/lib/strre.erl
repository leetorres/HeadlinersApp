% Taken from:
% http://onerlang.blogspot.se/2009/06/string-substitution-missing-in-erlang.html
% Changed to suit our needs.

-module(strre).
-author("Daniel Kwiecinski: lambder.com").
-purpose("string replace functionality").
-export([sub/3, gsub/3, replace/1]).

sub(Str,Old,New) ->
RegExp = "\\Q"++Old++"\\E",
re:replace(Str,RegExp,New,[multiline, {return, list}]).

gsub(Str,Old,New) ->
RegExp = "\\Q"++Old++"\\E",
re:replace(Str,RegExp,New,[global, multiline, {return, list}]).

replace(X) ->
    gsub(X, " ", "+").
