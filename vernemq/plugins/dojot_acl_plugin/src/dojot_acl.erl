-module(dojot_acl).


-export([
    check_topic/2,
    config_auth/2
]).

check_topic(Username, Topic) ->
    AttrsTopic = <<"attrs">>,
    ConfigTopic = <<"config">>,
    % Topic must be equal: username/attrs and username must be equal: tenant:deviceid
    [PaternOne|PaternTwo] = Topic,
    
    % we need to check if our mqtt client that was sent the message
    ResponseMatch = binary:match(Username, <<"mqtt-client-">>),

    [RestTopic| _] = PaternTwo,

    case {ResponseMatch, RestTopic} of
        {nomatch, _} ->
            case {PaternOne, RestTopic} of
                {Username, AttrsTopic} ->
                    next;
                {_, _} ->
                    error
            end;

        {_, ConfigTopic} ->
            ok
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
            error
    end.