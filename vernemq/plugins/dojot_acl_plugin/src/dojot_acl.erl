-module(dojot_acl).


-export([
    check_topic/2,
    config_auth/2
]).

check_topic(Username, Topic) ->

    % Topic must be equal: username/attrs and username must be equal: tenant:deviceid
    [PaternOne|PaternTwo] = binary:split(Topic, <<"/">>, [global]),

    if
        PaternOne == Username ->
            % here we check if topic is config
            if
                PaternTwo == <<"config">> ->
                    ok;
                true ->
                    next
            end;
        true ->
            error
    end.

config_auth(Username, Topic) ->
    % Topic must be equal: username/attrs and username must be equal: tenant:deviceid
    [PaternOne|PaternTwo] = binary:split(Topic, <<"/">>, [global]),

    if
        PaternOne == Username ->
            % here we check if topic is config
            if
                PaternTwo =:= <<"config">> ->
                    error;
                true ->
                    ok
            end;
        true ->
            error
    end.