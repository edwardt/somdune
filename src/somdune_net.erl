%%% Copyright 2010 Relaxed, Inc.
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.

% Initial code from http://lionet.livejournal.com/42016.html

-module(somdune_net).
-author('Jason Smith <jhs@couch.io>').

-include("somdune.hrl").
-export([proxy/2, request_to_binary/1]).


info(Msg, Args) -> error_logger:info_msg(Msg ++ "~n", Args).
error(Msg, Args) -> error_logger:error_msg(Msg ++ "~n", Args).


proxy(Port, Module) ->
    info("Starting proxy on port ~p with module ~p", [Port, Module]),
    case gen_tcp:listen(Port, [binary, {active, false}, {reuseaddr, true}]) of
        {ok, ListenSocket} ->
            tcpAcceptor(Port, ListenSocket, Module);
        {error, eaddrinuse} ->
            error("Port is already in use: ~p", [Port]),
            ok
    end.


tcpAcceptor(Port, ListeningSocket, BalancerModule) ->
        case gen_tcp:accept(ListeningSocket) of
                {ok, Sock} ->
                        Pid = spawn(fun () ->
                                receive
                                    permission ->
                                        inet:setopts(Sock, [
                                                binary,
                                                {packet, http_bin},
                                                {active, false}
                                        ])
                                    after 60000 -> timeout
                                end,
                                collectHttpHeaders(Sock, ?HTTP_HDR_RCV_TMO, BalancerModule, [])
                        end),

                        gen_tcp:controlling_process(Sock, Pid),
                        Pid ! permission,
                        tcpAcceptor(Port, ListeningSocket, BalancerModule);

                {error, econnaborted} ->
                        tcpAcceptor(Port, ListeningSocket, BalancerModule);
                {error, closed} -> finished;
                Msg ->
                        error_logger:error_msg("Acceptor died: ~p~n", [Msg]),
                        gen_tcp:close(ListeningSocket),
                        error_logger:error_msg("Attempting restart", []),
                        proxy(Port, BalancerModule)
        end.


collectHttpHeaders(Sock, UntilTS, BalancerModule, Headers) ->
  % TODO: Check the timestamp and reduce the timeout for subsequent calls so that the initial
  %       timeout provided is the total timeout of the collect run.
  %Timeout = (UntilTS - tstamp()),
  Timeout = UntilTS,

  inet:setopts(Sock, [{active, once}]),
  receive
    % Add this next header into the pile of already received headers
    {http, Sock, {http_header, _Length, Key, undefined, Value}} ->
        collectHttpHeaders(Sock, UntilTS, BalancerModule,
                [{header, {Key,Value}} | Headers]);

    {http, Sock, {http_request, Method, Path, HTTPVersion}} ->
        collectHttpHeaders(Sock, UntilTS, BalancerModule,
                [{http_request, Method, Path, HTTPVersion} | Headers]);

    {http, Sock, http_eoh} ->
        % With the headers known, allow the registered proxy module to decide what to do with the query.
        Packets = lists:reverse(Headers),
        case lists:keytake(http_request, 1, Packets) of
            {value, RequestPacket, HeaderPackets} ->
                {http_request, Method, Path, HttpVersion} = RequestPacket,
                RequestHeaders = [{atomify(Key), Val} || {header, {Key, Val}} <- HeaderPackets],
                Request = #request{socket=Sock, method=Method, path=Path, version=HttpVersion, headers=RequestHeaders},

                poison_pill(Request, <<"preroute">>),
                case apply(BalancerModule, route_request, [Request]) of
                    {route, {Host, Port}} ->
                        proxy(Request, Host, Port);
                    {route_new_request, {Host, Port}, NewRequest} ->
                        proxy(NewRequest, Host, Port);
                    {raw_route, {Host, Port}, Data} ->
                        info("raw_route~n~p", [Data]),
                        proxy_raw(Request, Data, Host, Port);
                    {reply, Status} ->
                        reply(Request, Status);
                    {reply, Status, RespHeaders} ->
                        reply(Request, Status, RespHeaders);
                    {reply, Status, RespHeaders, Body} ->
                        reply(Request, Status, RespHeaders, Body);
                    noop ->
                        ok;
                    Else ->
                        info("Unknown ~p:route_request for ~p -> ~p", [BalancerModule, Request, Else]),
                        ok
                end,
                poison_pill(Request, <<"postroute">>);
            false ->
                error("Failed to parse request: ~p", [Packets])
        end;

    {tcp_closed, Sock} -> nevermind;

    Msg -> io:format("Invalid message received: ~p~nAfter: ~p~n",
                [Msg, lists:reverse(Headers)])
  after Timeout ->
        reply(Sock, Headers,
                fun(_) -> [{status, 408, "Request Timeout"},
                        {header, {<<"Content-Type: ">>, <<"text/html">>}},
                        {html, "<html><title>Request timeout</title>"
                                "<body><h1>Request timeout</h1></body></html>"}]
        end)
  end.


reply(Request, Status) ->
    reply(Request, Status, []).

reply(Request, Status, Headers) ->
    {_, Message} = Status,
    reply(Request, Status, Headers, list_to_binary(Message)).

reply(Request, Status, Headers, Body) ->
    info("reply Status=~p Headers=~p Body=~p", [Status, Headers, Body]),
    poison_pill(Request, <<"prereply">>),

    {StatusCode, StatusMessage} = Status,
    StatusBytes = [
        case Request#request.version of
            {1, 1} -> <<"HTTP/1.1 ">>;
            _      -> <<"HTTP/1.0 ">>
        end,
        list_to_binary(integer_to_list(StatusCode)),
        <<" ">>,
        StatusMessage
    ],

    HeaderBytes = make_headers(lists:keystore('Content-Length', 1, Headers, {'Content-Length', integer_to_list(byte_size(Body))})),
    gen_tcp:send(Request#request.socket, [StatusBytes, <<"\r\n">>, HeaderBytes, Body]),
    poison_pill(Request, <<"postreply">>).


proxy(Req, Ip, Port) ->
    proxy_raw(Req, request_to_binary(Req), Ip, Port).

proxy_raw(Req, Data, Ip, Port) ->
    poison_pill(Req, <<"preproxy">>),
    { ok, ToSocket } = gen_tcp:connect(Ip, Port, [binary, {packet, 0} ]),
    info("Sending, ToSocket = ~p", [ToSocket]),
    gen_tcp:send(ToSocket, Data),
    relay(Req#request.socket, ToSocket, 0),
    poison_pill(Req, <<"postproxy">>).

make_request(Method, PathTuple, Version) ->
    {abs_path, Path} = PathTuple,

    [atom_to_list(Method),
     <<" ">>,
     Path,
     case Version of
        {1,1} -> <<" HTTP/1.1\r\n">>;
        _     -> <<" HTTP/1.0\r\n">>
     end].

make_headers(Headers) ->
    [ [ [atom_to_list(Key), <<": ">>, Value, <<"\r\n">> ] || {Key, Value} <- Headers ] , <<"\r\n">>].


request_to_binary(Req) ->
    list_to_binary([
        make_request(Req#request.method, Req#request.path, Req#request.version),
        make_headers(Req#request.headers) ]).


relay(FromSocket, ToSocket, Bytes) ->
    inet:setopts(FromSocket, [{packet, 0}, {active, once} ]),
    inet:setopts(ToSocket,   [{packet, 0}, {active, once} ]),
    receive
        {tcp, FromSocket, Data} ->
            gen_tcp:send(ToSocket, Data),
            relay(FromSocket, ToSocket, Bytes);
        {tcp, ToSocket, Data} ->
            gen_tcp:send(FromSocket, Data),
            relay(FromSocket, ToSocket, Bytes + size(Data));
        {tcp_closed, _} ->
            { ok, Bytes }
    after
        30000 ->
            { error, timeout }
    end.


atomify(Val) ->
    case is_binary(Val) of
        true -> list_to_atom(binary_to_list(Val));
        _    -> Val
    end.

% The ability to crash the server with a header for testing.
poison_pill(Request, Hook) ->
    case lists:keyfind('X-Somdune-Die', 1, Request#request.headers) of
        {'X-Somdune-Die', Hook} ->
            info("Activating poison pill at ~p", [Hook]),
            this = that;
        _ -> ok
    end.


% vim: sts=4 sw=4 et
