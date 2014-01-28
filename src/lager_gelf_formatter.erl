%% Copyright (c) 2014 Angelantonio Valente.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.

-module(lager_gelf_formatter).

-export([format/2, format/3]).

% ignore colors
format(Message, Config, _Colors) ->
    format(Message, Config).

format(Message, Config) ->
    Data = get_raw_data(Message, Config) ++ getvalue(extra_fields, Config, []),
    Json = jiffy:encode({Data}),
    compressed(Json, getvalue(compression, Config, gzip)).

get_raw_data(Message, Config) ->
    LongMessage = iolist_to_binary(lager_msg:message(Message)),

    ShortMessageSize = getvalue(short_message_size, Config, 80),
    ShortMessage = get_short_message(LongMessage, ShortMessageSize),

    HostName = get_host(getvalue(host, Config)),

    MetaData = lager_msg:metadata(Message),

    TS = unix_timestamp(lager_msg:timestamp(Message)),

    [{version,<<"1.0">>},
     {level, syslog_severity(lager_msg:severity(Message))},
     {short_message, ShortMessage},
     {full_message, LongMessage},
     {timestamp, TS},
     {line, getvalue(line, MetaData, -1)},
     {file, list_to_binary(atom_to_list(getvalue(module, MetaData)))},
     {host, list_to_binary(HostName)},
     {facility, list_to_binary(getvalue(facility, Config, "erlang"))},
     {'_from_pid', get_pid(getvalue(pid, MetaData))},
     {'_node', list_to_binary(atom_to_list(getvalue(node, MetaData)))},
     {'_application', list_to_binary(atom_to_list(getvalue(application, MetaData)))},
     {'_module', list_to_binary(atom_to_list(getvalue(module, MetaData)))},
     {'_function', list_to_binary(atom_to_list(getvalue(function, MetaData)))}].

get_pid(Pid) when is_pid(Pid) ->
    list_to_binary(pid_to_list(Pid));
get_pid(undefined) ->
    <<"unknown">>;
get_pid(Pid) when is_list(Pid) ->
    list_to_binary(Pid);
get_pid(_) ->
    <<"malformed">>.

syslog_severity(debug) ->
    7;
syslog_severity(info) ->
    6;
syslog_severity(notice) ->
    5;
syslog_severity(warning) ->
    4;
syslog_severity(error) ->
    3;
syslog_severity(critical) ->
    2;
syslog_severity(alert) ->
    1;
syslog_severity(emergency) ->
    0;
syslog_severity(_) ->
    7.

get_host(undefined) ->
    {ok, HostName} = inet:gethostname(),
    HostName;
get_host(HostName) ->
    HostName.

get_short_message(Msg, MaxSize) ->
    case size(Msg) =< MaxSize of 
        true -> 
            Msg;
        _ -> 
            <<SM:MaxSize/binary,_/binary>> = Msg,
            SM
    end.

compressed(Data, disabled) ->
    Data;
compressed(Data, gzip) ->
    zlib:gzip(Data);
compressed(Data, zlib) ->
    zlib:compress(Data).

getvalue(Tag, List, Default) ->
    case lists:keyfind(Tag, 1, List) of
        false ->
            Default;
        {Tag, Value} ->
            Value
    end.
getvalue(Tag, List) ->
    getvalue(Tag, List, undefined).

unix_timestamp({Mega, Sec, Micro}) ->
    Mega * 1000000 + Sec + Micro / 1000000.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_value_test_() -> 
    [
        ?_assertEqual(undefined, getvalue(test, [])),
        ?_assertEqual(undefined, getvalue(test, [{other, "test"}])),
        ?_assertEqual(ok, getvalue(test, [{other, "test"}], ok)),
        ?_assertEqual("test", getvalue(other, [{other, "test"}])),
        ?_assertEqual("test", getvalue(other, [{other, "test"}], "default"))
        ].

compressed_test_() -> 
    [
        ?_assertEqual("msg", compressed("msg", disabled)),
        ?_assertEqual(zlib:gzip("msg"), compressed("msg", gzip)),
        ?_assertEqual(zlib:compress("msg"), compressed("msg", zlib)),
        ?_assertException(error, function_clause, compressed("msg", unsupported))
        ].

get_short_message_test_() ->
    [
        ?_assertEqual(<<"a cut">>, get_short_message(<<"a cut message">>, 5)),
        ?_assertEqual(<<"not cut message">>, get_short_message(<<"not cut message">>, 80)),
        ?_assertException(error, badarg, get_short_message("not cut message", 80))
        ].

get_host_test_() ->
    [
        ?_assertEqual(localhost, get_host(localhost)),
        fun() ->
                    {ok, H} = inet:gethostname(),
                    ?assertEqual(H, get_host(undefined))
            end
        ].

syslog_severity_test_() ->
    [
        ?_assertEqual(7, syslog_severity(debug)),
        ?_assertEqual(6, syslog_severity(info)),
        ?_assertEqual(5, syslog_severity(notice)),
        ?_assertEqual(4, syslog_severity(warning)),
        ?_assertEqual(3, syslog_severity(error)),
        ?_assertEqual(2, syslog_severity(critical)),
        ?_assertEqual(1, syslog_severity(alert)),
        ?_assertEqual(0, syslog_severity(emergency)),
        ?_assertEqual(7, syslog_severity(unknown)),
        ?_assertEqual(7, syslog_severity("unknown"))
        ].

get_pid_test_() ->
    [
        ?_assertEqual(<<"<0.0.0>">>, get_pid(list_to_pid("<0.0.0>"))),
        ?_assertEqual(<<"unknown">>, get_pid(undefined)),
        ?_assertEqual(<<"<0.0.0>">>, get_pid("<0.0.0>")),
        ?_assertEqual(<<"malformed">>, get_pid(unknown))
        ].


unix_timestamp_test() ->
    TS = {1390,297427,346135},
    ?assertEqual(1390297427.346135, unix_timestamp(TS)).

get_raw_data_test() ->
    Now = os:timestamp(),
    MD = [{application, lager_graylog_backend}],
    Cfg = [{short_message_size, 6},
           {host, "localhost"},
           {inet_family, inet}],

    Message = lager_msg:new("a long message", Now, info, MD, []),
    Data = get_raw_data(Message, Cfg),

    Expected = [{version, <<"1.0">>},
                {level, 6},
                {short_message, <<"a long">>},
                {full_message, <<"a long message">>},
                {timestamp, unix_timestamp(Now)},
                {line, -1},
                {file, <<"undefined">>},
                {host, <<"localhost">>},
                {facility, <<"erlang">>},
                {'_from_pid', <<"unknown">>},
                {'_node', <<"undefined">>},
                {'_application', <<"lager_graylog_backend">>},
                {'_module', <<"undefined">>},
                {'_function', <<"undefined">>}
               ],

    ?assertEqual(Expected, Data).

format_2_test() ->
    Now = os:timestamp(),
    MD = [{application, lager_graylog_backend}],
    Cfg = [{short_message_size, 6},
           {host, "localhost"},
           {inet_family, inet},
           {compression, disabled}],

    Message = lager_msg:new("a long message", Now, info, MD, []),
    Data = format(Message, Cfg),

    Expected = jiffy:encode({[{version, <<"1.0">>},
                              {level, 6},
                              {short_message, <<"a long">>},
                              {full_message, <<"a long message">>},
                              {timestamp, unix_timestamp(Now)},
                              {line, -1},
                              {file, <<"undefined">>},
                              {host, <<"localhost">>},
                              {facility, <<"erlang">>},
                              {'_from_pid', <<"unknown">>},
                              {'_node', <<"undefined">>},
                              {'_application', <<"lager_graylog_backend">>},
                              {'_module', <<"undefined">>},
                              {'_function', <<"undefined">>}
                             ]}),

    ?assertEqual(Expected, Data).

format_3_test() ->
    Now = os:timestamp(),
    MD = [{application, lager_graylog_backend}],
    Cfg = [{short_message_size, 6},
           {host, "localhost"},
           {compression, disabled}],

    Message = lager_msg:new("a long message", Now, info, MD, []),
    Data = format(Message, Cfg, []),

    Expected = jiffy:encode({[{version, <<"1.0">>},
                              {level, 6},
                              {short_message, <<"a long">>},
                              {full_message, <<"a long message">>},
                              {timestamp, unix_timestamp(Now)},
                              {line, -1},
                              {file, <<"undefined">>},
                              {host, <<"localhost">>},
                              {facility, <<"erlang">>},
                              {'_from_pid', <<"unknown">>},
                              {'_node', <<"undefined">>},
                              {'_application', <<"lager_graylog_backend">>},
                              {'_module', <<"undefined">>},
                              {'_function', <<"undefined">>}
                             ]}),

    ?assertEqual(Expected, Data).

format_2_with_extra_fields_test() ->
    Now = os:timestamp(),
    MD = [{application, lager_graylog_backend}],
    Cfg = [{short_message_size, 6},
           {host, "localhost"},
           {inet_family, inet},
           {facility, "lager-test"},
           {extra_fields, [
               {'_environment', <<"test">>}
           ]},
           {compression, disabled}],

    Message = lager_msg:new("a long message", Now, info, MD, []),
    Data = format(Message, Cfg),

    Expected = jiffy:encode({[{version, <<"1.0">>},
                              {level, 6},
                              {short_message, <<"a long">>},
                              {full_message, <<"a long message">>},
                              {timestamp, unix_timestamp(Now)},
                              {line, -1},
                              {file, <<"undefined">>},
                              {host, <<"localhost">>},
                              {facility, <<"lager-test">>},
                              {'_from_pid', <<"unknown">>},
                              {'_node', <<"undefined">>},
                              {'_application', <<"lager_graylog_backend">>},
                              {'_module', <<"undefined">>},
                              {'_function', <<"undefined">>},
                              {'_environment', <<"test">>}
                             ]}),

    ?assertEqual(Expected, Data).

-endif.
