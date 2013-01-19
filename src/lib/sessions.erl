-module(sessions).
-compile(export_all).

-define(NewSessionId, "Searching").

login_status()->
    UniqueID = boss_session:get_session_data(?NewSessionId, "uniqueID"),
    boss_session:get_session_data(UniqueID, "uniqueID").

get_company()->
    UniqueID = boss_session:get_session_data(?NewSessionId, "uniqueID"),
    boss_session:get_session_data(UniqueID, "company").

get_email()->
    UniqueID = boss_session:get_session_data(?NewSessionId, "uniqueID"),
    boss_session:get_session_data(UniqueID, "email").

get_unique_id()->
    UniqueID = boss_session:get_session_data(?NewSessionId, "uniqueID"),
    boss_session:get_session_data(UniqueID, "uniqueID").

set_company(CompanyName)->
    UniqueID = boss_session:get_session_data(?NewSessionId, "uniqueID"),
    boss_session:set_session_data(UniqueID, "company", CompanyName).

set_email(Email)->
    UniqueID = boss_session:get_session_data(?NewSessionId, "uniqueID"),
    boss_session:set_session_data(UniqueID, "email", Email).

set_unique_id(UniqueID)->
    UniqueID = boss_session:get_session_data(?NewSessionId, "uniqueID"),
    boss_session:set_session_data(UniqueID, "uniqueID", UniqueID).

delete_unique_id()->
    UniqueID = boss_session:get_session_data(?NewSessionId, "uniqueID"),
    boss_session:remove_session_data(UniqueID, "uniqueID").

delete_company()->
    UniqueID = boss_session:get_session_data(?NewSessionId, "uniqueID"),
    boss_session:remove_session_data(UniqueID, "company").

delete_session()->
    UniqueID = boss_session:get_session_data(?NewSessionId, "uniqueID"),
    boss_session:delete_session(UniqueID).

set_search_word(SearchWord)->
    boss_session:set_session_data(?NewSessionId, "search word", SearchWord).

set_current_page(Page)->
    boss_session:set_session_data(?NewSessionId, "current page", Page).

set_total_pages(Pages)->
    boss_session:set_session_data(?NewSessionId, "total pages", Pages).

get_search_word()->
    boss_session:get_session_data(?NewSessionId, "search word").

get_current_page()->
    boss_session:get_session_data(?NewSessionId, "current page").

get_total_pages()->
    boss_session:get_session_data(?NewSessionId, "total pages").

    
    
    
    
