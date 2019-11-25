-module(dojot_acl).


-export([
    check_topic/2,
    config_auth/2
]).

check_topic(Username, Topic) ->
    ConfigTopic = <<"config">>,
    % Topic must be equal: username/attrs and username must be equal: tenant:deviceid
    [PaternOne|PaternTwo] = Topic,

    [RestTopic| _] = PaternTwo,

    case {PaternOne, RestTopic} of
        {Username, ConfigTopic} ->
            ok;
        {Username, _} ->
            next;
        {_, _} ->
            error
    end.

config_auth(Username, Topic) ->
    ConfigTopic = <<"config">>,

    % Topic must be equal: username/attrs and username must be equal: tenant:deviceid
    [Data|_] = Topic,
    {Data2, _} = Data,
    [PaternOne|PaternTwo] = Data2,
    [RestTopic| _] = PaternTwo,

    case {PaternOne, RestTopic} of
        {Username, ConfigTopic} ->
            ok;
        {_, _} ->
            error_logger:info_msg("testtt: ~p ~p", [PaternOne, RestTopic]),
            error
    end.