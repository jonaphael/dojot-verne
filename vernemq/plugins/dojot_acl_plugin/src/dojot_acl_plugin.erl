-module(dojot_acl_plugin).

-behaviour(auth_on_register_hook).
-behaviour(auth_on_subscribe_hook).
-behaviour(auth_on_publish_hook).

-export([auth_on_register/5,
         auth_on_publish/6,
         auth_on_subscribe/3]).

auth_on_register({_IpAddr, _Port} = Peer, {_MountPoint, _ClientId} = SubscriberId, UserName, Password, CleanSession) ->
    ok.

auth_on_publish(UserName, {_MountPoint, _ClientId} = SubscriberId, QoS, Topic, Payload, IsRetain) ->
    Result  = dojot_acl:check_topic(UserName, Topic),

    case Result of
        % the topic match with config
        ok ->
            ok;
        % the topic match is not config
        next ->
            next;
        % Username don't match
        error ->
            {error, notMatch}
    end.

auth_on_subscribe(UserName, ClientId, [{_Topic, _QoS}|_] = Topics) ->
    Result  = dojot_acl:config_auth(UserName, Topics),

    case Result of
        % the topic match with config
        ok ->
            ok;
        error ->
            %%The vernemq has a bug... even if this clause is activated, the verne return ok to the main app
            {error, notMatch}
    end.
