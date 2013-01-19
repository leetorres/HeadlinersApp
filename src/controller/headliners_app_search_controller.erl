%%%-------------------------------------------------------------------
%%% @author Lee "The Machine" Torres
%%% @copyright (C) 2012, Headliners
%%% @doc
%%% This module handles search request and initial data that is sent to the result page
%%% @end
%%%-------------------------------------------------------------------

-module(headliners_app_search_controller, [Req,SessionID]).
-compile(export_all).

-define(ITEMS_PER_PAGE,10).
-define(NewSessionId, "Searching").

% @doc
%Controller that routes the browser to /src/view/search/index.html, the homepage of
%the app
redirect('GET',[]) ->
    {action_other,[{action, "index"}]}.

% @doc
%Creates the session variable cookie and sends business intelligence data to the
%front page
index('GET',[]) ->
    boss_session:new_session(?NewSessionId),
    ID = sessions:get_unique_id(),

    LatestSearches = busintelstats:display_latest_searches(),
    TotalSearches = busintelstats:total_searches(),
    TotalTweets = busintelstats:total_tweets(),
    TweetPerSearch = io_lib:format("~.2f" ,[busintelstats:tweets_per_search()]),
    TotalCities = busintelstats:display_total_cities(),
    TotalCountries = busintelstats:display_total_countries(),
    {ok, [{searches, LatestSearches}, {totalSearches, TotalSearches}, 
	  {totalTweets, TotalTweets}, {tweetPerSearch, TweetPerSearch},
	  {uniqueID, ID},{totalCities, TotalCities}, {totalCountries, TotalCountries}]}.

% @doc
%Method that takes user search input that is entered in /src/view/search/index.html,
%and does a validity check on it, make sure it isn't a blank entry or more than
%25 chracters. If it passes the validity check, the search string is saved in the
%database, and a twitter and google search is done using the search string. Data
%is also returned that does business intelligence of the twitter and google searches.
%if the searchdoesn't pass the validity check, an error message is prepared. The user is then
%redirected to /src/view/search/result.html
result('POST', []) ->
    SearchString = Req:post_param("searchInput"),
    Latitude = geolocation:get_lat(Req:post_param("latitude")),
    Longitude = geolocation:get_long(Req:post_param("longitude")),
    case Latitude of
	"null" ->
	    City = "null",
	    Country = "null";
	_Other ->
	    {City, Country} = geolocation:get_city_country(Latitude,Longitude)
    end,
    Searched = search:new(id, SearchString, Latitude,Longitude, City, Country),
    case Searched:save() of
	{ok, SavedSearch} ->
	    proc:start_link(),
	    proc:query_twitter(SearchString),
	    proc:query_google(SearchString),
	    {redirect, [{action, "result"}, {id, SavedSearch:id()}]};
	{error, ErrorList} ->
	    {ok, [{errors, ErrorList}, {new_msg, Searched}]}
    end;

% @doc
%Method that pulls the latest search string query from the database (identified by the Id
%variable, and is used to make a Twitter and Google search. The resulting database
%information is then passed to /src/view/search/result.html
result('GET', [Id])->
    pagination:first_page_results(Id).
        
    


