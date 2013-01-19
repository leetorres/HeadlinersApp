%%% @author Lee "The Machine" Torres
%%% @copyright (C) 2012, Lee
%%% @doc 
%%% This module is used for rounding numbers 
%%% @end

-module(misc).
-compile(export_all).

ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

floor(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T - 1;
        Pos when Pos > 0 -> T;
        _ -> T
    end.
