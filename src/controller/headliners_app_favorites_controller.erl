%%%-------------------------------------------------------------------
%%% @author Lee, Chayi, Robin, David, Chris, Ashley
%%% @copyright (C) 2012, Headliners
%%% @doc
%%% This module is responisble for presenting favorite tweets
%%% @end
%%%-------------------------------------------------------------------

-module(headliners_app_favorites_controller, [Req]).
-compile(export_all).

-define(NewSessionId, "Searching").

% @doc
%Function responsible for redirecting a user to the favorites page
redirect('GET',[]) ->
    {action_other, [{action, "index"}]}.

% @doc
%Function that provides the data for the user's favorite tweets and
%the business intelligence for the company's favorites
index('GET',[])->
    LoginStatus = sessions:login_status(),
    case LoginStatus of
	undefined ->
	    {ok, [{uniqueID, LoginStatus}]};
	_LoggedIn ->
            %-------------------Getting session variables
	    Email = sessions:get_email(),
	    ID = sessions:get_unique_id(),
	    Company = sessions:get_company(),

            %-------------------Queries database for twitter data
	    UserSavedData = boss_db:find(saved_tweet, [{email, 'equals', Email}]),
	    Fromuser_idstr = [X:fromuser_idstr() || X <- UserSavedData ],

	    %Check if there are favorites available. If not, return a blank list,
	    %otherwise return the twitter user id list if available
	    FavoritesUnsorted = case Fromuser_idstr of
				     [] ->
					 [];
				     _Available ->
					 boss_db:find(tweet, [{fromuser_idstr, in, Fromuser_idstr}])
				end,
		
	    Favorites = pagination:get_first_page(lists:reverse(FavoritesUnsorted)),

            %---------------------Code requried for creating pagination
	    TwitterPages = misc:ceiling(length(FavoritesUnsorted)/10),
	    Twitter1stPage = pagination:page_selection(TwitterPages),

            %---------------------Creating Session Ids
	    sessions:set_current_page(1),
	    sessions:set_total_pages(TwitterPages),

	    %Retrieve the user's company name
	    GroupTotal = busintelstats:company_members(),

	    %If the user's company has no favorites, return information to the HTML without doing
	    %business intelligence. If the user's company has favorites, do business intelligence,
	    %and return the data to the html
	    case FavoritesUnsorted of
		[] ->
		    {ok, [{loginStatus, LoginStatus}, {pages, Twitter1stPage}, {favorites, Favorites},
			  {pageNumber, 0}, {totalPages, TwitterPages},{loginStatus, LoginStatus},
			   {email, Email}, {company, Company}, {groupTotal, GroupTotal}]};
		_HasCompanyFavorites ->
	            %---------------------Retrieving Business Intelligence
		    {{activity,{MA,A,VA,EA}}, {personality,{Casual,Striver,Regular,Star,Popular,Force}},
		     {inclusion, {OSI, SI, ESI}} } = busintelstats:twitter_favorite_review_batch(),
		    TotalFavorites = busintelstats:total_favorites(),
		    CompanyTotalFavorites = busintelstats:total_favorites_company(),
		    TotalFavoriteFollowers = busintelstats:total_favorite_followers(),
		    AverageFavoriteFollowers = busintelstats:average_favorite_followers(),

              	    %---------------------Returns data to index.html
		    {ok, [{loginStatus, LoginStatus}, {pages, Twitter1stPage}, {favorites, Favorites},
			  {pageNumber, 1}, {totalPages, TwitterPages},
			  {minimallyActive, MA}, {active, A}, {veryActive, VA}, {extremelyActive, EA},
			  {casualUser, Casual}, {striver, Striver}, {regularTweeter, Regular}, {star, Star},
			  {popularUser, Popular}, {steadyForce, Force}, {ocassionalSocial,OSI},
			  {sociallyIntegrated, SI}, {extremelySocial, ESI}, {loginStatus, LoginStatus},
			  {email, Email}, {company, Company}, {groupTotal, GroupTotal},
			  {totalFavorites, TotalFavorites},{companyTotalFavorites,CompanyTotalFavorites},
			  {totalFavoriteFollowers, TotalFavoriteFollowers},
			  {averageFavoriteFollowers,AverageFavoriteFollowers}]}
	    end	
    end.

    
