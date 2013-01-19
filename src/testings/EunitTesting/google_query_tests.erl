%%%-------------------------------------------------------------------
%%% @author Fan "Bear" Chayi
%%% @copyright (C) 2012, Bear
%%% @doc
%%% This module is used for testing the google_query.erl
%%% @end
%%% Manully input the SearchWord when try to call the testing function.
-module(google_query_tests).
-include_lib("eunit/include/eunit.hrl").
get_google_test()->
    ?assertMatch({ok,got_results},google_query:get_google()).
