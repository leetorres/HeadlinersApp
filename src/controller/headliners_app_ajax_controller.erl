%%%-------------------------------------------------------------------
%%% @author Robin and Lee
%%% @copyright (C) 2012, Headliners
%%% @doc
%%% This module handles the dynamic ajax calls from the user interface.
%%% @end
%%%-------------------------------------------------------------------
-module(headliners_app_ajax_controller, [Req,SessionID]).
-compile(export_all).

-define(NewSessionId, "Searching").

% @doc
%Function that is called by a Jquery Ajax function within /src/view/search/result.html. When a user
%clicks on a pagination option in result.html (i.e. Next, Previous, 1,2, etc), it removes all the 
%current tweets, and then calls this function to get a fresh batch of tweets, and is then reloaded
%within result.html. The function also returns other information, like what the next page will be
%in the pagination sequence, how manpy total pages the search query returns, and what pagination
%option was selected in order to trigger this function. The processing occurs within 
%pagination:tweets_pagination/4
get_tweet_page('GET',[Selection]) ->
    pagination:tweets_paginated(Selection).

% @doc
%Ajax function that returns paginated data for the favorites section
get_favorite_page('GET',[Selection]) ->
    pagination:favorites_paginated(Selection).

% @doc
% This function is called when you hit the Expand button on the search result page.
retrieve_twitter_user('GET', [UserId]) ->
    Data = case boss_db:find(twitter_user, [{id_str, 'matches',UserId}]) of
	       [] ->
		   intwitter_query:get_intwitter(UserId),
		   boss_db:find(twitter_user, [{id_str, 'matches',UserId}]);
	       Other -> Other
	   end,

    %---------------------Retrieving data on twitter user profiling
    {Activity, Personality, Inclusion}= busintelstats:twitter_user_review_singular(UserId),

    %---------------------JSON returning data to browser
    {json,[{twitter_user, Data},{activityLevel, Activity}, {personalityLevel, Personality},
	   {inclusionLevel, Inclusion}]}.

% @doc
% Function that is run when a user tries to logon. If the username and password is found,
% it changes the session variables to match the user's email. Otherwise, it returns a
% failure string for the user interface
login('GET',[Email, Password]) ->		       
    case boss_db:count(account, [{email, 'equals', Email},{password, 'equals',Password}]) of
	1 ->
	    
	    %-In the generic ?NewSessionID, add a value to uniqueID to indicate
            %that a user is logged in 
	    UniqueID = random:uniform(),
	    boss_session:new_session(UniqueID),
	    boss_session:set_session_data(?NewSessionId, "uniqueID", UniqueID), 
	    
	    %Retrieving company name
	    [CompanyName] = [X:company() ||  X <- boss_db:find(account, [{email, 'equals', Email}])],

	    %Setting session variables for the user
	    sessions:set_company(binary_to_list(CompanyName)),
	    sessions:set_email(Email),
	    sessions:set_unique_id(UniqueID),
	    
            %--------------------Return values in JSON format (list of the users favorites, indicate success)
	    Favorites = pagination:get_favorites(),
	    {json,[{result, "success"}, {favorites, Favorites}, {session, UniqueID}]};
	_Other ->
	    {json, [{result, "failure"}]}
    end.

% @doc
% Function that is run when a user tries to logout. Logging out removes the user email
% from the session variables
logout('GET',[]) ->

    %Remove the session ID for the user, and remove uniqueID to indicate that the user has logged out
    sessions:delete_company(),
    sessions:delete_unique_id(),
    sessions:delete_session(),
    boss_session:remove_session_data(?NewSessionId, "uniqueID"),
    {json,[{result,"success"}]}.

% @doc
%This function is called when you hit the register button on the register page.
%It will take the values of the email, password and company fields and store them in the database.
register('GET', [Email, Password, Company]) ->
    io:format("~p~n~p~n~p", [Email, Password, Company]),
    case boss_db:count(account, [{email, 'equals', Email}, {password, 'equals', Password}]) of
	0 ->
	    accounts:create_account(Email, Password, Company), %calls a function that will do the database operations
	    {json,[{result, "success"}]};
	_Other ->
	    {json,[{result, "failure"}]}
	end.


% @doc
% Function that is responsible for adding a tweet to the saved_tweets table, when
% a user requests it to be saved as a favorite. (for example hitting the favorite button)
tweet_favorite('GET',[Id_str]) ->
    Email = sessions:get_email(),
    case Email of
	undefined ->
	    {json, [{result, "not_logged_in"}]};
	_LoggedIn ->
	    case tweet_already_saved(Id_str, Email) of
		true ->
		    {json, [{result, "already_saved"}]};
		false ->
		    Data = boss_db:find(tweet, [{id_str, 'matches', Id_str}]),
		    [Fromuser_idstr] = [X:fromuser_idstr()   || X <- Data ],
		    Favorite = saved_tweet:new(id, Id_str, Fromuser_idstr, Email ),
		    Favorite:save(),
		    
		    boss_db:execute("UPDATE tweets SET likes = likes + 1 WHERE id_str = " ++ Id_str),

		    {json, [{result, "success"}]}
	    end
    end.

% @doc
%Function that is responsible for removing a tweet to the saved_tweets table, when
%a user requests it to be unfavorited
%Same as above but when removing a favourite
tweet_unfavorite('GET',[Id_str]) ->
    Email = sessions:get_email(),
    case Email of
	"undefined" ->
	    {json, [{result, "not_logged_in"}]};
	_LoggedIn ->
	    case tweet_already_saved(Id_str, Email) of
		true ->
		    [{saved_tweet,SavedId,_Id_str, _fromuser_idstr,_user}] = 
			boss_db:find(saved_tweet,[{id_str, 'equals', Id_str}, {email, 'equals', Email}]),
		    boss_db:delete(SavedId),

%		    boss_db:execute("UPDATE tweets SET likes = likes - 1 WHERE id_str = " ++ Id_str),
		    
		    {json, [{result, "deleted"}]};
		false ->
		    {json, [{result, "already_deleted"}]}
	    end
    end.
    
% @doc
%Helper function that checks to see if tweet is already favorited by the current user
tweet_already_saved(Id_str, Email) ->
    case boss_db:count(saved_tweet, [{id_str, 'equals', Id_str},
			       {email, 'equals', Email}]) of
	1 ->
	    true;
	_0 ->
	    false
    end.

% @doc
% Returns the favorite tweets of the logged in user. If the user is not
% logged in, it returns a blank list
get_favorites('GET',[]) ->
    Email = sessions:get_email(),
    Favorites = case Email of
		    "undefined" ->
			[];
		    _LoggedIn ->
			boss_db:find(saved_tweet, [{email, 'equals', Email}])
		end,
    {json, [{favorites, Favorites}]}.
