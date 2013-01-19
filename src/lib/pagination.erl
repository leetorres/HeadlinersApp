%%%-------------------------------------------------------------------
%%% @author Robin, Lee, Chayi, Chris, Ashley, David
%%% @copyright (C) 2012, Headliners
%%% @doc
%%% This module handles pagination and a few database opeartions.
%%% @end
%%%-------------------------------------------------------------------
-module(pagination).
-compile(export_all).

% Sets the number of tweets or google results for each page
-define(ITEMS_PER_PAGE,10). 
% Words to ignore when querying the database
-define(WORDS_TO_IGNORE, ["if", "for", "the", "when", "how", "so", "is", "do", "in", "to", "what", "why", "when", "a", "an"]).
-define(NewSessionId, "Searching").

% @doc
% Breaks down a string into a list of all the words in that string
breakdown([]) ->
    [];
breakdown(List) ->
    breakdown(List, [], []).
breakdown([$ |T], NewWord, NewList) ->
    breakdown(T, [], NewList ++ [NewWord]);
breakdown([H|T], NewWord, NewList) ->
    breakdown(T, NewWord ++ [H], NewList);
breakdown([], NewWord, NewList) ->
    NewList ++ [NewWord].

% @doc
% Checks if a list contains any of the words mentioned in the defined 'WORDS_TO_IGNORE' and removes it from the list
cleanList(List) ->
    [X || X <- List, not(lists:member(X, ?WORDS_TO_IGNORE))].

% @doc
% Queries the google table in the database for all results that contains any of the words in a list
google_db_query([]) ->
    [];
google_db_query(List) ->
    google_db_query(cleanList(List), []).
google_db_query([H|T], Results) ->
    google_db_query(T, Results ++ boss_db:find(google, [{title, 'matches', "*" ++ H}]));
google_db_query([], Results) ->
    Results.

% @doc
% Returns the first page of results (Twitter, Google, and Business Analytics) for index.html
first_page_results(Id) ->
   %-------------------Queries database for twitter and google data
    {search, _Id, SearchText, Lat, Long, _City, _Country} = boss_db:find(Id),
    SearchNoCaseSensitivity = binary_to_list(SearchText),
    TwitterResultsUnsorted = boss_db:find(tweet, [{text, 'matches', "*" ++ SearchNoCaseSensitivity}]),
    GoogleResultsUnsorted = google_db_query(breakdown(SearchNoCaseSensitivity)),
    TwitterResults = get_first_page(lists:reverse(TwitterResultsUnsorted)),
    GoogleResults = get_first_page(lists:reverse(GoogleResultsUnsorted)),

    %---------------------Code requried for creating pagination
    TwitterPages = misc:floor(length(TwitterResultsUnsorted)/10),
    Twitter1stPage = pagination:page_selection(TwitterPages),

    %---------------------Creating Session Ids
    boss_session:get_session_data(?NewSessionId),
    sessions:set_search_word(binary_to_list(SearchText)),
    sessions:set_current_page(1),
    sessions:set_total_pages(TwitterPages),

    %---------------------Creating Business Intelligence Data
    AverageFollowers = busintelstats:average_followers(SearchNoCaseSensitivity),
    TotalFollowers = busintelstats:total_followers(SearchNoCaseSensitivity),
    {{activity,{MA,A,VA,EA}}, {personality,{Casual,Striver,Regular,Star,Popular,Force}},
     {inclusion, {OSI, SI, ESI}} } = busintelstats:twitter_user_review_batch(SearchNoCaseSensitivity),
    MostLikedTweets = busintelstats:most_liked_tweets(),


    %---------------------Checking user login status
    LoginStatus = sessions:login_status(),
    case LoginStatus of
	undefined ->	    
    
            %---------------------Returns data to result.html
	    {ok,[{tweets,TwitterResults}, {googles, GoogleResults}, {pageNumber, 1}, {totalPages, TwitterPages},
		 {pages, Twitter1stPage}, {latitude, Lat}, {longitude, Long},
		 {searchWord, boss_session:get_session_data(?NewSessionId, "search word")}, 
		 {averageFollowers, AverageFollowers}, {totalFollowers, TotalFollowers},
		 {minimallyActive, MA}, {active, A}, {veryActive, VA}, {extremelyActive, EA},
		 {casualUser, Casual}, {striver, Striver}, {regularTweeter, Regular}, {star, Star},
		 {popularUser, Popular}, {steadyForce, Force}, {ocassionalSocial,OSI},
		 {sociallyIntegrated, SI}, {extremelySocial, ESI}, {loginStatus, LoginStatus},
		 {favorites, get_favorites()},{mostLikedTweets, MostLikedTweets}]};
	_LoggedIn ->
	    
	    {ok,[{tweets,TwitterResults}, {googles, GoogleResults}, {pageNumber, 1}, {totalPages, TwitterPages},
		 {pages, Twitter1stPage}, {latitude, Lat}, {longitude, Long},
		 {searchWord, boss_session:get_session_data(?NewSessionId, "search word")}, 
		 {averageFollowers, AverageFollowers}, {totalFollowers, TotalFollowers},
		 {minimallyActive, MA}, {active, A}, {veryActive, VA}, {extremelyActive, EA},
		 {casualUser, Casual}, {striver, Striver}, {regularTweeter, Regular}, {star, Star},
		 {popularUser, Popular}, {steadyForce, Force}, {ocassionalSocial,OSI},
		 {sociallyIntegrated, SI}, {extremelySocial, ESI}, {loginStatus, LoginStatus},
		 {favorites, get_favorites()},{mostLikedTweets, MostLikedTweets}]}
    end.

% @doc
% Returns data for pagination, specifically data for helping to dynamically display a set page of Twitter data
tweets_paginated(Selection) ->
    SearchWord = sessions:get_search_word(),
    CurrentPage = sessions:get_current_page(),
    TotalPages = sessions:get_total_pages(),
    LoginStatus = sessions:login_status(),
	
    SearchString = "*" ++ SearchWord,
    Offset =  CurrentPage* ?ITEMS_PER_PAGE,

    case Selection of
	"Next" ->
	    RawTweets = boss_db:find(tweet, [{text, 'matches',SearchString}]),
	    Count = length(RawTweets),
	    if
		Count < 20 ->
		    Tweets = lists:sublist((lists:reverse(RawTweets)),
					   10,10),
		    sessions:set_current_page(CurrentPage+1),
		    {json, [{currentPage, CurrentPage+1},{totalPages,TotalPages}, {tweet,Tweets}, {selection,Selection},
			    {loginStatus, LoginStatus},{favorites, get_favorites()}] };
		Count > 20 ->
		    Tweets = lists:sublist((lists:reverse(RawTweets)),
					   Offset + 10,10),
		    sessions:set_current_page(CurrentPage+1),
		    {json, [{currentPage, CurrentPage+1},{totalPages,TotalPages}, {tweet,Tweets}, {selection,Selection},
			    {loginStatus, LoginStatus},{favorites, get_favorites()}] }
		end;
	"Previous" ->
	    case CurrentPage of
		2 ->
		    Tweets = lists:sublist((lists:reverse(boss_db:find(tweet, [{text, 'matches',SearchString}]))),
					   1,10),
		    sessions:set_current_page(CurrentPage-1),
		    {json, [{currentPage, CurrentPage-1},{totalPages,TotalPages}, {tweet,Tweets},{selection,Selection},
			    {loginStatus, LoginStatus},{favorites, get_favorites()}] };

		_Other ->
		    Tweets = lists:sublist((lists:reverse(boss_db:find(tweet, [{text, 'matches',SearchString}]))),
					   Offset-10,10),
		    sessions:set_current_page(CurrentPage-1),
		    {json, [{currentPage, CurrentPage-1},{totalPages,TotalPages}, {tweet,Tweets},{selection,Selection},
			    {loginStatus, LoginStatus},{favorites, get_favorites()}] }
		
		end;
	_NumberSelection ->
	    case Selection of
		"1" ->
		    Tweets = lists:sublist((lists:reverse(boss_db:find(tweet, [{text, 'matches',SearchString}]))),
					   1, 10),
		    sessions:set_current_page(list_to_integer(Selection)),
		    {json, [{currentPage, list_to_integer(Selection)},{totalPages,TotalPages}, {tweet,Tweets},{selection,Selection},
			    {loginStatus, LoginStatus},{favorites, get_favorites()}] };

		_OtherNumber ->
		    
		    NewOffset = list_to_integer(Selection) * ?ITEMS_PER_PAGE,
		    Tweets = lists:sublist((lists:reverse(boss_db:find(tweet, [{text, 'matches',SearchString}]))),
					   NewOffset, 10),
		    sessions:set_current_page(list_to_integer(Selection)),
		    {json, [{currentPage, list_to_integer(Selection)},{totalPages,TotalPages}, {tweet,Tweets},{selection,Selection},
			    {loginStatus,LoginStatus},{favorites, get_favorites()}] }
	    end
    end.

% @doc
%Function that returns tweets in paginated form, found in the favorites page
favorites_paginated(Selection) ->

    %---------------------Getting Session Ids
    CurrentPage = sessions:get_current_page(),
    TotalPages = sessions:get_current_page(),
    Email = sessions:get_email(),

    %-------------------Queries database for twitter data
    UserSavedData = boss_db:find(saved_tweet, [{email, 'equals', Email}]),
    Fromuser_idstr = [X:fromuser_idstr() || X <- UserSavedData ],
    Favorites = boss_db:find(tweet, [{fromuser_idstr, in, Fromuser_idstr}]),
    Offset =  CurrentPage* ?ITEMS_PER_PAGE,

    case Selection of
	"Next" ->
	    Count = length(Favorites),
	    if
		Count < 20 ->
		    Tweets = lists:sublist((lists:reverse(Favorites)),
				   10,10),
		    sessions:set_current_page(CurrentPage+1),
		    {json, [{currentPage, CurrentPage+1},{totalPages,TotalPages}, {tweet,Tweets}, {selection,Selection},
			    {email, Email},{favorites, get_favorites()}] };
		Count > 20 ->
		    Tweets = lists:sublist((lists:reverse(Favorites)),
				   Offset+10,10),
		    sessions:set_current_page(CurrentPage+1),
		    {json, [{currentPage, CurrentPage+1},{totalPages,TotalPages}, {tweet,Tweets}, {selection,Selection},
			    {email, Email},{favorites, get_favorites()}] }

		end;
	"Previous" ->
	    case CurrentPage of
		2 ->
		    Tweets = lists:sublist((lists:reverse(Favorites)),
					   1,10),
		    sessions:set_current_page(CurrentPage-1),
		    {json, [{currentPage, CurrentPage-1},{totalPages,TotalPages}, {tweet,Tweets},{selection,Selection},
			    {email, Email},{favorites, get_favorites()}] };

		_Other ->
		    Tweets = lists:sublist((lists:reverse(Favorites)),
					   Offset-10,10),
		    sessions:set_current_page(CurrentPage-1),
		    {json, [{currentPage, CurrentPage-1},{totalPages,TotalPages}, {tweet,Tweets},{selection,Selection},
			    {email, Email},{favorites, get_favorites()}] }
		
		end;
	_NumberSelection ->
	    case Selection of
		"1" ->
		    Tweets = lists:sublist((lists:reverse(Favorites)),
					   1, 10),
		    sessions:set_current_page(list_to_integer(Selection)),
		    {json, [{currentPage, list_to_integer(Selection)},{totalPages,TotalPages}, {tweet,Tweets},{selection,Selection},
			    {email, Email},{favorites, get_favorites()}] };

		_OtherNumber ->
		    
		    NewOffset = list_to_integer(Selection) * ?ITEMS_PER_PAGE,
		    Tweets = lists:sublist((lists:reverse(Favorites)),
					   NewOffset, 10),
		    sessions:set_current_page(list_to_integer(Selection)),
		    {json, [{currentPage, list_to_integer(Selection)},{totalPages,TotalPages}, {tweet,Tweets},{selection,Selection},
			    {email, Email},{favorites, get_favorites()}] }
	    end
    end.


% @doc
% Returns the number of pages to display on bottom pagination bar for Twitter results
page_selection(Pages) ->
    if 
	Pages > 10 -> 
	    pages_to_display(Pages);
	Pages < 1 ->
	    [];
	Pages < 10 ->
	    pages_to_display(Pages)
    end.

% @doc
% Helper function for page_selection/1    
pages_to_display(Pages) when Pages >= 10 ->
    lists:seq(2,10);
pages_to_display(Pages) ->
    lists:seq(2, misc:ceiling(Pages)).

% @doc
% Function that takes a result of a database query (i.e. query for Twitter or Google results), 
% and returns the first 10 results, in order to create a first page in a pagination sequence
% on the results page
get_first_page(List)->
    lists:sublist(List, 1,10).

% @doc
% Returns the list of favorite Tweets for a user that is logged in, otherwise if they are not
% logged in, it returns a blank list
get_favorites() ->
    Email = sessions:get_email(),
    case Email of
	"undefined" ->
	    [];
	_LoggedIn ->
	    boss_db:find(saved_tweet, [{email, 'equals', Email}])
    end.
