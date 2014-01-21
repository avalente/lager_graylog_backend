lager_graylog_backend
+++++++++++++++++++++

Overview
--------

`Lager <https://github.com/basho/lager>`_ backend for `Graylog <http://graylog2.org>`_.
This backend routes log messages to the `graylog` log server.

Usage
-----

Include this backend into your project using rebar::

    {lager_graylog_backend, ".*", {git, "https://github.com/avalente/lager_graylog_backend.git", "master"}}

Then you need to add a new "handler" in lager configuration, usually in your `app.config` file, for example::

    {lager, [
        {handlers, [
            {lager_graylog_backend, [
                {host, "<graylog_host>:<graylog_port>"},
                    {level, info}, 
                    {name, graylog2},
                    {format_config, [
                        {facility, "<facility>"}
                        ]}
                ]}
            ]}
        ]}

Test
----

This backend has a quite complete suite test, you can run it with rebar::

    $ ./rebar get-deps
    $ ./rebar compile
    $ ./rebar eunit skip_deps=true

License
-------

Apache 2.0
