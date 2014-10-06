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

-module(lager_logstash_formatter).

-export([format/2, format/3]).

% ignore colors
format(Message, Config, _Colors) ->
    format(Message, Config).

format(Message, Config) ->
    Data = get_raw_data(Message, Config) ++ getvalue(extra_fields, Config, []),
    Msg = jiffy:encode({Data}),
    <<Msg/binary,"\n">>.

utc_iso_datetime(Message) ->
    Ts = {_, _, Micro} = lager_msg:timestamp(Message),

    {UTCDate, {H, M, S}} = calendar:now_to_universal_time(Ts),

    UTCTime = {H, M, S, Micro div 1000 rem 1000},

    {RawDate, RawTime} = lager_util:format_time({UTCDate, UTCTime}),
    iolist_to_binary(io_lib:format("~sT~sZ", [RawDate, RawTime])).

    
get_raw_data(Message, Config) ->
    LongMessage = iolist_to_binary(lager_msg:message(Message)),

    HostName = get_host(getvalue(host, Config)),

    MetaData = lager_msg:metadata(Message),

    [{'@version',<<"1">>},
     {severity_label, binary_severity(lager_msg:severity(Message))},
     {message, LongMessage},
     {'@timestamp', utc_iso_datetime(Message)},
     {host, list_to_binary(HostName)},
     {facility_label, list_to_binary(getvalue(facility, Config, "user-level"))},
     {'application', list_to_binary(atom_to_list(getvalue(application, MetaData)))},

     {erlang, {[
         {line, getvalue(line, MetaData, -1)},
         {file, list_to_binary(atom_to_list(getvalue(module, MetaData)))},
         {'from_pid', get_pid(getvalue(pid, MetaData))},
         {'node', list_to_binary(atom_to_list(getvalue(node, MetaData)))},
         {'module', list_to_binary(atom_to_list(getvalue(module, MetaData)))},
         {'function', list_to_binary(atom_to_list(getvalue(function, MetaData)))}]}}].


get_pid(Pid) when is_pid(Pid) ->
    list_to_binary(pid_to_list(Pid));
get_pid(undefined) ->
    <<"unknown">>;
get_pid(Pid) when is_list(Pid) ->
    list_to_binary(Pid);
get_pid(_) ->
    <<"malformed">>.

binary_severity(debug) ->
    <<"debug">>;
binary_severity(info) ->
    <<"informational">>;
binary_severity(notice) ->
    <<"notice">>;
binary_severity(warning) ->
    <<"warning">>;
binary_severity(error) ->
    <<"error">>;
binary_severity(critical) ->
    <<"critical">>;
binary_severity(alert) ->
    <<"alert">>;
binary_severity(emergency) ->
    <<"emergency">>;
binary_severity(_) ->
    <<"debug">>.

get_host(undefined) ->
    {ok, HostName} = inet:gethostname(),
    HostName;
get_host(HostName) ->
    HostName.

getvalue(Tag, List, Default) ->
    case lists:keyfind(Tag, 1, List) of
        false ->
            Default;
        {Tag, Value} ->
            Value
    end.
getvalue(Tag, List) ->
    getvalue(Tag, List, undefined).

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

get_host_test_() ->
    [
        ?_assertEqual(localhost, get_host(localhost)),
        fun() ->
                    {ok, H} = inet:gethostname(),
                    ?assertEqual(H, get_host(undefined))
            end
        ].

binary_severity_test_() ->
    [
        ?_assertEqual(<<"debug">>, binary_severity(debug)),
        ?_assertEqual(<<"informational">>, binary_severity(info)),
        ?_assertEqual(<<"notice">>, binary_severity(notice)),
        ?_assertEqual(<<"warning">>, binary_severity(warning)),
        ?_assertEqual(<<"error">>, binary_severity(error)),
        ?_assertEqual(<<"critical">>, binary_severity(critical)),
        ?_assertEqual(<<"alert">>, binary_severity(alert)),
        ?_assertEqual(<<"emergency">>, binary_severity(emergency)),
        ?_assertEqual(<<"debug">>, binary_severity(unknown)),
        ?_assertEqual(<<"debug">>, binary_severity("unknown"))
        ].

get_pid_test_() ->
    [
        ?_assertEqual(<<"<0.0.0>">>, get_pid(list_to_pid("<0.0.0>"))),
        ?_assertEqual(<<"unknown">>, get_pid(undefined)),
        ?_assertEqual(<<"<0.0.0>">>, get_pid("<0.0.0>")),
        ?_assertEqual(<<"malformed">>, get_pid(unknown))
        ].


get_raw_data_test() ->
    Now = os:timestamp(),
    MD = [{application, lager_graylog_backend}],
    Cfg = [{host, "localhost"},
           {inet_family, inet}],

    Message = lager_msg:new("a message", Now, info, MD, []),
    Data = get_raw_data(Message, Cfg),

    Expected = [{'@version', <<"1">>},
                {severity_label, <<"informational">>},
                {message, <<"a message">>},
                {'@timestamp', utc_iso_datetime(Message)},
                {host, <<"localhost">>},
                {facility_label, <<"user-level">>},
                {'application', <<"lager_graylog_backend">>},
                {erlang, {[
                    {line, -1},
                    {file, <<"undefined">>},
                    {'from_pid', <<"unknown">>},
                    {'node', <<"undefined">>},
                    {'module', <<"undefined">>},
                    {'function', <<"undefined">>}]}}
               ],

    ?assertEqual(Expected, Data).

format_2_test() ->
    Now = os:timestamp(),
    MD = [{application, lager_graylog_backend}],
    Cfg = [{host, "localhost"},
           {inet_family, inet}],

    Message = lager_msg:new("a message", Now, info, MD, []),
    Data = format(Message, Cfg),

    Expected = jiffy:encode({[{'@version', <<"1">>},
                              {severity_label, <<"informational">>},
                              {message, <<"a message">>},
                              {'@timestamp', utc_iso_datetime(Message)},
                              {host, <<"localhost">>},
                              {facility_label, <<"user-level">>},
                              {'application', <<"lager_graylog_backend">>},
                              {erlang, {[
                                  {line, -1},
                                  {file, <<"undefined">>},
                                  {'from_pid', <<"unknown">>},
                                  {'node', <<"undefined">>},
                                  {'module', <<"undefined">>},
                                  {'function', <<"undefined">>}]}}
                             ]}),

    ?assertEqual(<<Expected/binary,"\n">>, Data).

format_3_test() ->
    Now = os:timestamp(),
    MD = [{application, lager_graylog_backend}],
    Cfg = [{host, "localhost"}],

    Message = lager_msg:new("a message", Now, info, MD, []),
    Data = format(Message, Cfg, []),

    Expected = jiffy:encode({[{'@version', <<"1">>},
                              {severity_label, <<"informational">>},
                              {message, <<"a message">>},
                              {'@timestamp', utc_iso_datetime(Message)},
                              {host, <<"localhost">>},
                              {facility_label, <<"user-level">>},
                              {'application', <<"lager_graylog_backend">>},
                              {erlang, {[
                                  {line, -1},
                                  {file, <<"undefined">>},
                                  {'from_pid', <<"unknown">>},
                                  {'node', <<"undefined">>},
                                  {'module', <<"undefined">>},
                                  {'function', <<"undefined">>}]}}
                             ]}),

    ?assertEqual(<<Expected/binary,"\n">>, Data).

format_2_with_extra_fields_test() ->
    Now = os:timestamp(),
    MD = [{application, lager_graylog_backend}],
    Cfg = [{host, "localhost"},
           {inet_family, inet},
           {facility, "lager-test"},
           {extra_fields, [
               {'extra', <<"test">>}
        ]}],

    Message = lager_msg:new("a message", Now, info, MD, []),
    Data = format(Message, Cfg),

    Expected = jiffy:encode({[{'@version', <<"1">>},
                              {severity_label, <<"informational">>},
                              {message, <<"a message">>},
                              {'@timestamp', utc_iso_datetime(Message)},
                              {host, <<"localhost">>},
                              {facility_label, <<"lager-test">>},
                              {'application', <<"lager_graylog_backend">>},
                              {erlang, {[
                                  {line, -1},
                                  {file, <<"undefined">>},
                                  {'from_pid', <<"unknown">>},
                                  {'node', <<"undefined">>},
                                  {'module', <<"undefined">>},
                                  {'function', <<"undefined">>}]}},
                              {'extra', <<"test">>}
                             ]}),

    ?assertEqual(<<Expected/binary,"\n">>, Data).

-endif.
