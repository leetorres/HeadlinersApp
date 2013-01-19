%%%-------------------------------------------------------------------
%%% @author Fan "Bear" Chayi
%%% @copyright (C) 2012, Bear
%%% @doc
%%% This module is used for testing functions in busintelstats.erl
%%% @end
-module(busintelstats_tests).
-include_lib("eunit/include/eunit.hrl").
 total_tweets_test()->
    ?assert(is_integer(busintelstats:total_tweets())).

 tweets_per_search_test()->
    ?assert(is_integer(busintelstats:tweets_per_search())).
 
 display_latest_searches_test()->
    ?assertMatch(X when is_integer(X) ; X>=0,length(busintelstats:display_latest_searches())). 

 display_latest_cities_test()->
    ?assertMatch(X when is_integer(X) ; X>=0,length(busintelstats:display_latest_cities())).
 
 display_total_cities_test()->
    ?assertMatch(X when is_integer(X) ; X>=0,length(busintelstats:display_total_cities())).
 
 display_latest_countries_test()->
    ?assertMatch(X when is_integer(X) ; X>=0,length(busintelstats:display_latest_countries())).
 
 display_total_countries_test()->
    ?assertMatch(X when is_integer(X) ; X>0,length(busintelstats:display_total_countries())).
 
 percentage_location_not_share_test()->
    ?assertMatch(X when X=<1; X>0 ,length(busintelstats:percentage_location_not_share())). 
 
 percentage_location_share_test()->
    ?assertMatch(X when X=<1 ; X>0,length(busintelstats:percentage_location_share())).
 
 average_followers_test()->
    %%manually give SearchWord in case of testing this function seperately
    %%manually test the lists:sum(FollowerList)/Count>=0 or not
    ?assertMatch(X when X>=0 ; X<1, busintelstats:average_followers()).
 
 total_followers_test()->
    %%manually give SearchWord in case of testing this function seperately
     ?assertMatch(X when is_integer(X) ; X>=0,busintelstats:total_followers()). 

 average_public_lists_test()->
    %%manually give SearchWord in case of testing this function seperately
    %%manually test the lists:sum(PublicList)/Count>=0 or not
    ?assertMatch(X when X>=0 ; X<1, busintelstats:average_followers()). 			     
 activity_level_test()->
    %%manually give Followers in case of testing this function seperately
    ?assert(is_list(busintelstats:activity_level())).
 
 twitter_personality_test()->
    %%manually give (Followers,Following) in case of testing this function seperately
    ?assert(is_list(busintelstats:twitter_personality())).
  
 inclusion_level_test()->
    %%manually give PublicCount in case of testing this function seperately
    ?assert(is_list(busintelstats:inclusion_level())).
 
 twitter_user_review_singular_test()->
    %%manually give UserId in case of testing this function seperately
    ?assertMatch({ActivityLevel,Personality,Inclusion} when is_list(ActivityLevel) ; is_list(Personality); is_list(Inclusion),busintelstats:twitter_user_review_singular()).
 
 twitter_user_review_batch_test()->
     %%manually give SearchWord in case of testing this function seperately
    ?assertMatch({ActivityCount,PersonalityCount,InclusionCount} when is_integer(ActivityCount) ; ActivityCount>=0 ; is_integer(PersonalityCount) ; PersonalityCount>=0 ; is_integer(InclusionCount) ; InclusionCount>=0,busintelstats:twitter_user_review_batch()).
 
 activity_count_test()->
   %%manually give Ids
   ?assert(is_integer(busintelstats:acitvity_count())).    
 
 ersonality_count_test()->
   %%manually give Ids
   ?assert(is_integer(busintelstats:personality_count())).    

 inclusion_count_test()->
   %%manually give Ids
   ?assert(is_integer(busintelstats:inclusion_count())).    
