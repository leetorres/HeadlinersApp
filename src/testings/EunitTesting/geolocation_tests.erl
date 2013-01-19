%%%-------------------------------------------------------------------
%%% @author Fan "Bear" Chayi
%%% @copyright (C) 2012, Bear
%%% @doc
%%% This module is used for testing functions in the geolocation.erl
%%% @end
-module(geolocation_tests).
-include_lib("eunit/include/eunit.hrl").
get_lat_test()->
    %%manully give the Lat
    ?assertMatch(X when is_integer(X) or  X=:="null",geolocation:get_lat()),
    ?assertMatch(X when is_integer(X) or  X=:="null",geolocation:get_lat()).
get_long_test()->
    %%manully give the Long
     ?assertMatch(X when is_integer(X) or X=:="null",geolocation:get_long()),
     ?assertMatch(X when is_integer(X) or X=:="null",geolocation:get_long()).
get_cit_country_test()->
    %%manully give the (Lat,Long)
    ?assertMatch({ok,_},geolocation:get_cit_country()),
    ?assertMatch({ok,_},geolocation:get_cit_country()).
build_geolocation_url_test()->
    %%mamully give the (Lat,Long)
    ?assertEqual("e",lists:last(geolocation:build_geolocation_url())).
parse_data_test()->
     %%manully give the Lists in case of running this test.
    ?assertMatch(X when is_list(X),geolocation:parse_data([])).
get_results_test()->
    %%manully give the {struct,Data}
    ?assertMatch({City,Country} when is_list(City) ;is_list(Country),geolocation:get_results()).
