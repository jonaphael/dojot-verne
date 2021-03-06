# Dojot ACL Plugin

This plugin enable the ACL for dojot. The important files are:

- src/dojot_acl.erl 
- src/dojot_acl.app.src

This plugin use Erlang OTP.


You must have a recent version of Erlang installed (it's recommended to use the
same one VerneMQ is compiled for, typically > 17). To compile do:

    ./rebar3 compile

Then enable the plugin using:

    vmq-admin plugin enable --name dojot_acl_plugin --path <PathToYourPlugin>/dojot_acl_plugin/_build/default

Depending on how VerneMQ is started you might need ``sudo`` rights to access ``vmq-admin``.
Moreover the ``<PathToYourPlugin>`` should be accessible by VerneMQ (file permissions).

Since this plugin implements hooks which are already covered by
``vmq_passwd`` and ``vmq_acl`` you might want to disable these in order to see
the effect of this plugin.

    vmq-admin plugin disable --name vmq_passwd
    vmq-admin plugin disable --name vmq_acl
