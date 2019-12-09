-module(clean_sess).
-define(MAX_TIMEOUT, erlang:list_to_integer(os:getenv("LIFETIME_SESSION"))).


-export([
    set_connection_timeout/1,
    disconnect_client/1
]).

disconnect_client(SubId) ->
    error_logger:info_msg("disconnect_client: ~p ~n", [SubId]),
    vernemq_dev_api:disconnect_by_subscriber_id(SubId, []).


set_connection_timeout(SubId) ->
    timer:apply_after(?MAX_TIMEOUT, ?MODULE, disconnect_client, [SubId]),
    ok.