lager_graylog_backend
+++++++++++++++++++++

Overview
--------

`Lager <https://github.com/basho/lager>`_ backend for `Graylog <http://graylog2.org>`_.
This backend routes log messages to the `graylog` log server.

.. image:: https://travis-ci.org/avalente/lager_graylog_backend.png?branch=master 
    :target: https://travis-ci.org/avalente/lager_graylog_backend
    :alt: Build status

Usage
-----

Include this backend into your project using rebar::

    {lager_graylog_backend, ".*", {git, "https://github.com/avalente/lager_graylog_backend.git", "master"}}

Then you need to add a new "handler" in lager configuration, usually in your `app.config` file, for example::

    {lager, [
        {handlers, [
            {lager_graylog_backend, [
                {host, "localhost:12201"},
                    {level, info}, 
                    {name, graylog2},
                    {format_config, [
                        {facility, "myapp"},
                        {extra_fields, [
                            {'_environment', <<"production">>}
                        ]}
                    ]}
                ]}
            ]}
        ]}

Configuration
-------------

Backend configuration parameters:

 * host: graylog server with explicit port number (usually 12201) (string, required)
 * level: minimum logging level - messages below that level will be dropped (atom, required)
 * name: backend name (atom, required)
 * format_config: backend-specific configuration - a proplist with:

     * facility: logging facility (string, defaults to "erlang")
     * short_message_size: maximum length for the "short message" field (integer, defaults to 80)
     * compression: one of disabled, gzip, zlib (atom, defaults to gzip)
     * host: source hostname, defaults to the local host's name (string)
     * extra_fields: optional proplist of {name, value}, they will be sent as "additional fields" to graylog. The name must be an atom starting with an underscore, while the value must be a binary.



Lager
-----

`lager_graylog_backend` works with Lager version 2.0 or above.

Test
----

This backend has a quite complete suite test, you can run it with rebar::

    $ ./rebar get-deps
    $ ./rebar compile
    $ ./rebar eunit skip_deps=true

License
-------

Apache 2.0
