# Dojot Disconnect Plugin

This plugin check and close staled connections for dojot. The important files are:

- src/dojot_acl.erl 
- src/dojot_disconnect_plugin.app.src

This plugin use Erlang OTP.


You must have a recent version of Erlang installed (it's recommended to use the
same one VerneMQ is compiled for, typically > 17). To compile do:

    ./rebar3 compile

Then enable the plugin using:

    vmq-admin plugin enable --name dojot_disconnect_plugin --path <PathToYourPlugin>/dojot_disconnect_plugin/_build/default

Depending on how VerneMQ is started you might need ``sudo`` rights to access ``vmq-admin``.
Moreover the ``<PathToYourPlugin>`` should be accessible by VerneMQ (file permissions).

Since this plugin implements hooks which are already covered by
``vmq_passwd`` and ``vmq_acl`` you might want to disable these in order to see
the effect of this plugin.

    vmq-admin plugin disable --name vmq_passwd
    vmq-admin plugin disable --name vmq_acl

The environment variable ``LIFETIME_SESSION`` tells to the plugin the maximum lifetime the oppened session (pub or sub) must have. After the timeout, the plugin will disconnect the created client by his id. You MUST set this env var in your yml file (kubernetes, etc..)