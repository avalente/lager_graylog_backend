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

-module(lager_graylog_backend).

-behaviour(gen_event).

-export([init/1, handle_call/2, handle_event/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {name, address, port, socket, level, formatter=lager_gelf_formatter, format_config, extra_fields}).

-include_lib("lager/include/lager.hrl").

%
% Lager backend for graylog
%
% The backend must be configured in the lager's configuration section:
%
% {lager_graylog_backend, [
%    {host, "<graylog_host>:<graylog_port>"},
%    {level, info}, 
%    {name, graylog2},
%    {format_config, [
%        {facility, "<facility>"}
%    ]}
%  ]}
%

init(Config)->
    {Level, FormatConfig, InetFamily, {Host, Port}, ExtraFields, Name} = get_config(Config),

    {ok,Address} = inet:getaddr(Host, InetFamily),

    % active=false since we never want to receive.  Buffer will fill and packets
    % will start bouncing
    {ok,Socket} = gen_udp:open(0,[binary,{active,false}]),

    {ok, #state{level=lager_util:level_to_num(Level), 
        name={?MODULE,Name},
        address=Address,
        port=Port,
        socket=Socket,
        format_config=FormatConfig,
        extra_fields=ExtraFields}
    }.


%% @private

handle_call({set_loglevel, Level}, #state{name=Ident} = State) ->
    case validate_loglevel(Level) of
        false ->
            {ok, {error, bad_loglevel}, State};
        Levels ->
            ?INT_LOG(notice, "Changed loglevel of ~s to ~p", [Ident, Level]),
            {ok, ok, State#state{level=Levels}}
    end;
handle_call(get_loglevel, #state{level=Level} = State) ->
    {ok, Level, State};
handle_call(_Request, State) ->
    {ok, ok, State}.

%% @private

handle_event({log, Message}, #state{name=N, level=L, formatter=F, format_config=FC,
                                    socket=S, address=A, port=P} = State) ->
    case lager_util:is_loggable(Message, L, {?MODULE, N}) of
        true ->
            Msg = F:format(Message, FC),
            %TODO: support chunked GELF (http://graylog2.org/gelf#specs)
            gen_udp:send(S, A, P, Msg),
            
            {ok, State};
        false ->
            {ok, State}
    end;
  
handle_event(_Event, State) ->
    {ok, State}.


%% @private
handle_info(_Info, State) ->
    {ok, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

get_config(Config) ->
    Level = proplists:get_value(level, Config, info),
    FormatConfig = proplists:get_value(format_config, Config, []),
    InetFamily = proplists:get_value(inet_family, Config, inet),
    {Host, Port} = get_host(proplists:get_value(host, Config, undefined)),
    ExtraFields = proplists:get_value(extra_fields, Config, []),
    
    Name = proplists:get_value(name, Config, {Host, Port}),  
    
    check_config({level, Level}),
    check_config({inet_family, InetFamily}),

    {Level, FormatConfig, InetFamily, {Host, Port}, ExtraFields, Name}.


check_config({inet_family,F}) when F =/= inet6 andalso F=/= inet ->
    throw({bad_config, bad_inet_family, F});
check_config({level,L}) -> case
    lists:member(L,?LEVELS) of 
        true -> 
            true;
        _ -> 
            throw({bad_config, invalid_level, L}) 
    end;
check_config(_) ->
    true.

validate_loglevel(Level) ->
    try lager_util:config_to_mask(Level) of
        Levels ->
            Levels
    catch
        _:_ ->
            false
    end.

get_host(undefined) ->
    throw({bad_config, host_not_found});
get_host(FullHost) ->
    Tokens = string:tokens(FullHost, ":"),
    case Tokens of
        [Host] ->
            throw({bad_config, missing_gelf_port, Host});
        [Host, Port] ->
            case catch(list_to_integer(Port)) of
                NPort when is_integer(NPort) ->
                    {Host, NPort};
                _ ->
                    throw({bad_config, non_integer_port})
            end;
        _ ->
            throw({bad_config, invalid_host, FullHost})
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(PORT, 12201).

init_test() ->
    Config = [{host, "localhost:12201"}],
    Res = init(Config),

    ?assertMatch({ok, #state{level=64, name={?MODULE, {"localhost", 12201}},
                           address={127,0,0,1}, port=12201,
                           socket=_, format_config=[], extra_fields=[]}}, Res).

handle_call_set_loglevel_test_() ->
    [
        ?_assertEqual({ok, {error, bad_loglevel}, #state{}}, handle_call({set_loglevel, undefined}, #state{})),
        ?_assertEqual({ok, ok, #state{level=lager_util:config_to_mask(info)}}, handle_call({set_loglevel, info}, #state{}))
        ].

handle_call_get_loglevel_test() ->
    ?assertEqual({ok, info, #state{level=info}}, handle_call(get_loglevel, #state{level=info})).

handle_call_cathcall_test() ->
    ?assertEqual({ok, ok, #state{}}, handle_call(test, #state{})).

udp_server(Caller) ->
    {ok, Socket} = gen_udp:open(?PORT, [binary, {active, once}]),
    receive
        {udp, Socket, _Host, _Port, Bin} ->
            Caller ! {data, Bin}
    end,
    gen_udp:close(Socket).

functional_test() ->
    Cfg = [{lager_graylog_backend, [
                    {host, "localhost:"++integer_to_list(?PORT)},
                    {level, info}, 
                    {name, graylog2},
                    {format_config, [
                            {facility, "lager-test"}
                            ]}
                    ]}],

    application:load(lager),
    application:set_env(lager, handlers, Cfg),

    lager:start(),

    % wait for a bit to allow lager to be fully started
    timer:sleep(500),

    Self = self(),
    F = fun() -> udp_server(Self) end,

    spawn_link(F),

    lager:log(info, self(), "This is a test"),

    {Res} = receive
        {data, Bin} ->
            jiffy:decode(zlib:gunzip(Bin))
    after 500 ->
            {error, message_not_received}
    end,

    ?assertEqual(<<"1.0">>, proplists:get_value(<<"version">>, Res)),
    ?assertEqual(<<"This is a test">>, proplists:get_value(<<"short_message">>, Res)),
    ?assertEqual(<<"This is a test">>, proplists:get_value(<<"full_message">>, Res)),
    ?assertEqual(6, proplists:get_value(<<"level">>, Res)).

-endif.
