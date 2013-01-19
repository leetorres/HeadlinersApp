%%%-------------------------------------------------------------------
%%% @author Fan "Bear" Chayi
%%% @copyright (C) 2012, Bear
%%% @doc
%%% This module is used for testing the intwitter_query.erl
%%% @end
%%% Manully input the TwitterId_str when call this testing function
-module(intwitter_query_tests).
-include_lib("eunit/include/eunit.hrl").
get_intwitter_test()->
     ?assertMatch({ok,got_results},intwitter_query:get_intwitter()).
    
