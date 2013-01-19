%%%-------------------------------------------------------------------
%%% @author Robin, Lee, Chayi, Chris, Ashley, David
%%% @copyright (C) 2012, Headliners
%%% @doc
%%% This module contains all of the business intelligence calculations.
%%% @end
%%%-------------------------------------------------------------------

-module(busintelstats).
-compile(export_all).

-define(NewSessionId, "Searching").

%Returns the # of searches done by the application
total_searches()->
    boss_db:count(search,[]).

%Returns the # of tweets saved in the DB
total_tweets()->
    boss_db:count(tweet,[]).

%Returns the average number of tweets found for each search term in the database
tweets_per_search()->    
    total_tweets()/total_searches().

%Displays the last 5 search terms
display_latest_searches() ->
    lists:sublist(lists:reverse(boss_db:find(search,[])),1,5).

%Displays the last 5 user cities    
display_latest_cities()->
    List = [binary_to_list(X:city()) || X <- boss_db:find(search, [])],
    lists:sublist(lists:reverse(List),1,5).

%Displays the # of cities that users have been searching from
display_total_cities() ->
    List = [binary_to_list(X:city()) || X <- boss_db:find(search, [])],
    CleanList = sets:to_list(sets:from_list(List)),
    length(CleanList) -1.

%Displays the last 5 countries that users have searched from
display_latest_countries() ->
    List = [binary_to_list(X:country()) || X <- boss_db:find(search, [])],
    lists:sublist(lists:reverse(List),1,5).

%Displays the # of cities that users have been searching from
display_total_countries() ->
    List = [binary_to_list(X:country()) || X <- boss_db:find(search, [])],
    CleanList = sets:to_list(sets:from_list(List)),
    length(CleanList) -1.

%Returns how many users have refused to share their location        
percentage_location_not_share()->
    boss_db:count(search,[{city,'matches',"null"}])/total_searches()*100.

%Returns how many users have agreed to share their location
percentage_location_share()->
    100 - percentage_location_not_share().

%Returns the number of people in a company
company_members() ->
    Company = sessions:get_company(),
    boss_db:count(account, [{company, 'equals', Company}]).

%Returns the average followers per user (Twitter) for an organization's favorites
average_followers(SearchWord) ->
    Ids = [X:fromuser_idstr() || X <- boss_db:find(tweet, [{text, 'matches', SearchWord}])],
    case Count = length(Ids) of
	0 ->
	    1;
	Length ->
	    Length
    end,       
    case Ids of
	[] ->
	    0;
	_Active ->
	    FollowerList = [list_to_integer(binary_to_list(X:followers_count())) || X <- boss_db:find(twitter_user, [{id_str, in, Ids}]) ],
	    io_lib:format("~.2f",[lists:sum(FollowerList)/Count])
    end.

%Returns the average followers per user (Twitter) for a companie's favorites
total_favorite_followers() ->
    Company = sessions:get_company(),
    Emails = [X:email() ||  X <- boss_db:find(account, [{company, 'equals', Company}])],
    SavedTweets = boss_db:find(saved_tweet, [{email, in, Emails}]),
    TweetIds = [X:id_str()|| X <- SavedTweets],
    case TweetIds of
	[] ->
	    0;
	_Available ->
	    Favorites = boss_db:find(tweet, [{id_str, in, TweetIds}]),
	    FollowersIds = [X:fromuser_idstr() ||X <- Favorites],
	    FollowersCountList = [list_to_integer(binary_to_list(X:followers_count()))||X <- boss_db:find(twitter_user, [{id_str, in, FollowersIds}])],
	    lists:sum(FollowersCountList)
    end.

average_favorite_followers() ->
        Company = sessions:get_company(),
    Emails = [X:email() ||  X <- boss_db:find(account, [{company, 'equals', Company}])],
    SavedTweets = boss_db:find(saved_tweet, [{email, in, Emails}]),
    TweetIds = [X:id_str()|| X <- SavedTweets],
    case TweetIds of
	[] ->
	    0;
	_Available ->
	    Favorites = boss_db:find(tweet, [{id_str, in, TweetIds}]),
	    FollowersIds = [X:fromuser_idstr() ||X <- Favorites],
	    FollowersCountList = [list_to_integer(binary_to_list(X:followers_count()))||X <- boss_db:find(twitter_user, [{id_str, in, FollowersIds}])],
	    Count = length(FollowersCountList),
	    io_lib:format("~.2f",[lists:sum(FollowersCountList)/Count])
    end.

%Returns the total # of followers for a given Twitter search term. 
total_followers(SearchWord) ->
    Ids = [X:fromuser_idstr() || X <- boss_db:find(tweet, [{text, 'matches', SearchWord}])],
    case Ids of 
	[] ->
	    0;
	_Active ->
	    FollowerList = [list_to_integer(binary_to_list(X:followers_count())) || X <- boss_db:find(twitter_user, [{id_str, in, Ids}]) ],
	    lists:sum(FollowerList)
    end.

%Returns the total # of favorites made by the user
total_favorites() ->
    Email = sessions:get_email(),
    length(boss_db:find(saved_tweet, [{email, 'equals', Email}])).

%Returns the total # of favorites made by the company
total_favorites_company() ->
    Company = sessions:get_company(),
    Emails = [X:email() ||  X <- boss_db:find(account, [{company, 'equals', Company}])],
    length(boss_db:find(saved_tweet, [{email, in, Emails}])).

%Returns the average # of public lists that users belong to for a given Twitter search term
average_public_lists(SearchWord) ->
    Ids = [X:fromuser_idstr() || X <- boss_db:find(tweet, [{text, 'matches', SearchWord}])],
    Count = length(Ids),
    PublicList = [list_to_integer(binary_to_list(X:listed_count())) || X <- boss_db:find(twitter_user, [{id_str, in, Ids}]) ],
    io_lib:format("~.2f",[lists:sum(PublicList)/Count]).

% Determines a Twitter user's "Momentum Rating"
% This is one of our proprietary algorithms 
% The algorithm determines the users activity level. 
activity_level(-1)  ->
    "Unknown";
activity_level(Followers) when Followers =< 100 ->
    "Minimally Active";
activity_level(Followers) when Followers > 100, Followers =< 1000 ->
    "Active";
activity_level(Followers) when Followers > 1000, Followers < 5000 ->
    "Very Active";
activity_level(Followers) when Followers > 5000 ->
    "Extremely Active".

% Determines a Twitter user's "Twitter Profile" or personality
% This is one of our proprietary algorithms
% The algorithm determines users "personality"
twitter_personality(-1, -1)->
    "Unknown";
twitter_personality(Followers, _Following) when Followers =< 100 ->
    "Casual User";
twitter_personality(Followers, Following) when Followers =< 5000, Following > Followers ->
    "Striver";
twitter_personality(Followers, _Following) when Followers =< 5000 ->
    "Regular Tweeter";
twitter_personality(Followers, Following) ->
    Ratio = Followers/Following,
    if
	Ratio > 5 ->
	    "Star";
	Ratio > 1 ->
	    "Popular User";
	Ratio < 1 ->
	    "Steady Force"
    end.

% Determines a Twitter User's "Sociability Rating"
% This is one of our proprietary algorithms
% The algorithm determines how socially active users are
inclusion_level(-1) ->
    "Unknown";
inclusion_level(PublicCount) when PublicCount =< 100->
    "Ocassional Social Involvement";
inclusion_level(PublicCount) when PublicCount > 100, PublicCount =< 200 ->
    "Socially Integrated";
inclusion_level(_PublicCount) ->
    "Extremely Socially Integrated".

% Returns business intelligence for one Twitter user
% This functions uses our proprietary algorithms
% It is called when you click the Expand button on the search results page.
twitter_user_review_singular(UserId) ->
    Data = boss_db:find(twitter_user, [{id_str, 'matches',UserId}]),

    %---------------------Retrieving twitter user values
    [Followers] = [list_to_integer(binary_to_list(X:followers_count())) || X <- Data ],
    [Following] = [list_to_integer(binary_to_list(X:following())) || X <- Data ],
    [PublicCount] = [list_to_integer(binary_to_list(X:listed_count())) || X <- Data ],

    %---------------------Retrieving twitter user ranking based on scaling system
    ActivityLevel = activity_level(Followers),
    Personality = twitter_personality(Followers, Following),
    Inclusion = inclusion_level(PublicCount),
    
    {ActivityLevel, Personality, Inclusion}.

% Returns business intelligence for all the Twitter user's that have tweeted about a given search term
twitter_user_review_batch(SearchWord) ->    
    Ids = [X:fromuser_idstr() || X <- boss_db:find(tweet, [{text, 'matches', SearchWord}])],
    ActivityCount = activity_count(Ids),
    PersonalityCount = personality_count(Ids),
    InclusionCount = inclusion_count(Ids),
    {ActivityCount, PersonalityCount, InclusionCount}.

% Returns business intelligence for all the Twitter user's that have been favorited by user groups.
twitter_favorite_review_batch() ->
    Company = sessions:get_company(),
    Emails = [X:email() ||  X <- boss_db:find(account, [{company, 'equals', Company}])],
    SavedTweets = boss_db:find(saved_tweet, [{email, in, Emails}]),
    TweetIds = [X:id_str()|| X <- SavedTweets],
    Favorites = boss_db:find(tweet, [{id_str, in, TweetIds}]),
    UserIds = [X:fromuser_idstr() ||X <- Favorites],
    
    %------------------Business intelligence functions and returning values
    ActivityCount = activity_count(UserIds),
    PersonalityCount = personality_count(UserIds),
    InclusionCount = inclusion_count(UserIds),
    {ActivityCount, PersonalityCount, InclusionCount}.

% Buffer function for activity_count/2
activity_count(Ids) ->
    activity_count(Ids, {activity, {0,0,0,0}}).

% A recursive algorithm that determines the total "Momenum Ratings" 
% for Twitter users tweeting about a particular search term
activity_count([], {activity, {MA, A, VA, EA}}) ->    
    {activity, {MA, A, VA, EA}};
activity_count([UserId| Tail], {activity,{MA, A, VA, EA}}) -> 
    Data = boss_db:find(twitter_user, [{id_str, 'matches',UserId}]),

    %---------------------Retrieving twitter user values
    [Followers] = case [list_to_integer(binary_to_list(X:followers_count())) || X <- Data ] of
		    [] ->
			[-1];
		    NonBlank ->
			NonBlank
	     end,
    
    Result = activity_level(Followers),
    case Result of
	"Unknown" ->
	    activity_count(Tail, {activity,{MA, A, VA, EA}});
	"Minimally Active" ->
	    activity_count(Tail, {activity,{MA +1, A, VA, EA}});
	"Active" ->
	    activity_count(Tail, {activity,{MA, A+1, VA, EA}});
	"Very Active" ->
	    activity_count(Tail, {activity,{MA, A, VA +1, EA}});
	"Extremely Active" ->
	    activity_count(Tail, {activity,{MA, A, VA, EA +1}})
    end.

%Buffer function for personality_count/2
personality_count(Ids) ->
    personality_count(Ids, {personality,{0,0,0,0,0,0}}).

% A recursive algorithm that determines the total "Personality Profile" 
% for Twitter users tweeting about a particular search term
personality_count([], {personality,{Casual,Striver,Regular,Star,Popular, Force}}) ->
    {personality,{Casual,Striver,Regular,Star,Popular, Force}};
personality_count([UserId | Tail], {personality,{Casual,Striver,Regular,Star,Popular, Force}}) ->
    Data = boss_db:find(twitter_user, [{id_str, 'matches',UserId}]),

    %---------------------Retrieving twitter user values
    [Followers] = case [list_to_integer(binary_to_list(X:followers_count())) || X <- Data ] of
		      [] -> [-1];
		      FollowerResult -> FollowerResult
		  end,
    
    [Following] = case [list_to_integer(binary_to_list(X:following())) || X <- Data ] of
		      [] -> [-1];
		      FollowingCount -> FollowingCount
		  end,

    Result = twitter_personality(Followers, Following),
    case Result of
	"Unknown" ->
	    personality_count(Tail, {personality,{Casual,Striver,Regular,Star,Popular, Force}});
	"Casual User" ->
	    personality_count(Tail, {personality,{Casual +1,Striver,Regular,Star,Popular, Force}});
	"Striver" ->
	    personality_count(Tail, {personality,{Casual,Striver +1,Regular,Star,Popular, Force}});
	"Regular Tweeter" ->
	    personality_count(Tail, {personality,{Casual,Striver,Regular+1,Star,Popular, Force}});
	"Star" ->
	    personality_count(Tail, {personality,{Casual,Striver,Regular,Star +1,Popular, Force}});
	"Popular User" ->
	    personality_count(Tail, {personality,{Casual,Striver,Regular,Star,Popular +1, Force}});
	"Steady Force" ->
	    personality_count(Tail, {personality,{Casual,Striver,Regular,Star,Popular, Force+1}})
    end.

%Buffer function for inclusion_count/2
inclusion_count(Ids) ->
    inclusion_count(Ids, {inclusion,{0, 0, 0}}).

% A recursive algorithm that determines the total "Sociability Rating" 
% for Twitter users tweeting about a particular search term
inclusion_count([], {inclusion,{OSI, SI, ESI}}) ->
    {inclusion,{OSI, SI, ESI}};
inclusion_count([UserId|Tail], {inclusion,{OSI, SI, ESI}}) ->
    Data = boss_db:find(twitter_user, [{id_str, 'matches',UserId}]),
    
    %---------------------Retrieving twitter user values
    [PublicCount] = case[list_to_integer(binary_to_list(X:listed_count())) || X <- Data ] of
			[] ->
			    [-1];
			PublicResult ->
			    PublicResult
	     end,
    
    Result = inclusion_level(PublicCount),
    case Result of 
	"Unknown" ->
	    inclusion_count(Tail, {inclusion,{OSI, SI, ESI}});
	"Ocassional Social Involvement" ->
	    inclusion_count(Tail, {inclusion,{OSI +1, SI, ESI}});
	"Socially Integrated" ->
	    inclusion_count(Tail, {inclusion,{OSI, SI +1, ESI}});
	"Extremely Socially Integrated" ->
	    inclusion_count(Tail, {inclusion,{OSI, SI, ESI+1}})
    end.

% Returns a tweet from the database for a specific tweet ID
tweet_from_id(ID) ->
    boss_db:find(tweet, [{id, 'matches', ID}]).

% Returns a list with the 'ID' of the five most liked (or favorited) tweets in our 
%database in descending order. The IDs in the list is the IDs from the first column 
%in the tweets table in the database.
most_liked_tweet_id() ->
    {data,{mysql_result,_,[[Elem1|_],[Elem2|_],[Elem3|_],[Elem4|_],[Elem5|_]],0,0,[]}} 
	= boss_db:execute("SELECT * FROM tweets ORDER BY likes DESC LIMIT 5;"),
    [Elem1, Elem2, Elem3, Elem4, Elem5].

% Returns the 5 most liked tweets
most_liked_tweets() ->
    Tweets = most_liked_tweet_id(),
    boss_db:find(tweet, [{id, in, Tweets}]).

% Same as above but returns a specified number of tweets.
% Returns a list with the 'ID' of the most liked (or favorited) tweets in our database in descending order.
% The IDs in the list is the IDs from the first column in the tweets table in the database.
most_liked_tweet_id(NumberOfResults) ->
    {data,{mysql_result,_,Data,0,0,[]}} 
	= boss_db:execute("SELECT * FROM tweets ORDER BY likes DESC LIMIT " ++ integer_to_list(NumberOfResults) ++ ";"),
    get_list_of_ids(Data, []).

% Helper function to extract only the IDs from the result of most_liked_tweet_id/1
get_list_of_ids([H|T], List) ->
    [ID|_] = H,
    get_list_of_ids(T, List ++ [ID]);
get_list_of_ids([], List) ->
    List.

    
