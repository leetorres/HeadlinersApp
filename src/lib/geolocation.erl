%%%-------------------------------------------------------------------
%%% @author Lee
%%% @copyright (C) 2012, Headliners
%%% @doc
%%% This module is taking care of location data and translates long/lat into streets, cities or the like.
%%% @end
%%%-------------------------------------------------------------------
-module(geolocation).
-compile(export_all).

%Returns the latitude of a user. If they refuse to provide their location, it returns
%"null"
get_lat(Latitude) ->
    case Latitude of
	"" ->
	    "null";
	Lat ->
	    Lat	       
    end.
%Returns the longitude of a user. If they refuse to provide their location, it returns
%"null"
get_long(Longitude) ->
    case Longitude of
	"" ->
	    "null";
	Long ->
	    Long
    end.

%Returns the city and country of a user, in the form of  {City, Country}
get_city_country(Lat, Long) ->
    inets:start(),
    case httpc:request(get, {build_geolocation_url(Lat, Long),[]},[{timeout, 6000}],[]) of
	{ok, {_,_, Body}} -> parse_data(Body);
	Other -> {error, Other} 
    end.

%Builds the REST URL for a location request to google maps
build_geolocation_url(Lat, Long) -> 
    "http://maps.googleapis.com/maps/api/geocode/json?latlng=" ++ Lat ++ 

	"," ++ Long ++ "&sensor=false".

%Parses data from JSON format
parse_data(Data) ->
    Result = get_results(mochijson2:decode(Data)),
    Result.

%Pattern matches geolocation results out of a tuple
get_results({struct, Data}) ->
    X = proplists:get_value(<<"results">>, Data, not_found),
    address_comp(X).

%Pulls information out of "address_components" in a JSON file
address_comp([{struct,Data}|_T]) ->
    X = proplists:get_value(<<"address_components">>, Data, not_found),
    io:format("~p~n",[X]),
    City = get_city(X),
    Country = get_country(X),
    io:format("~p~n~p~n",[City,Country]),
    {City, Country}.

%Buffer function for get_city/2
get_city(X) ->
    get_city(X, not_found).

%Pulls out user city from a JSON file
get_city([], City) ->
    City;
get_city([{struct, Values}|T], _City) ->
    case proplists:get_value(<<"types">>, Values, not_found) of
	[<<"administrative_area_level_1">>, <<"political">>] ->
	    CityName = proplists:get_value(<<"long_name">>, Values, not_found),
	    get_city([], binary_to_list(CityName));
	_Other ->
	    get_city(T, not_found)
    end.

%Buffer function for get_country/2
get_country(X) ->
    get_country(X, not_found).

%Pulls out country from a JSON file
get_country([], Country) ->
    io:format("~p~n ",[Country]),
    Country;
get_country([{struct, Values}|T], _Country) ->
    case proplists:get_value(<<"types">>, Values, not_found) of
	[<<"country">>,<<"political">>] ->
	    CountryName = proplists:get_value(<<"long_name">>, Values, not_found),
	    get_country([], binary_to_list(CountryName));
	_Other ->
	    get_country(T, not_found)
    end.
	
    
    
    

