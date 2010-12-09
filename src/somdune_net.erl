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
-author('Jason Smith <jhs@couchone.com>').

-include("somdune.hrl").
-export([run_proxy/3, request_to_binary/1, tcp_setopts/2, tcp_send/2]).
-export([log_info/2, log_error/2, header/2]).


log_info(Msg, Args) -> error_logger:info_msg(Msg ++ "~n", Args).
log_error(Msg, Args) -> error_logger:error_msg(Msg ++ "~n", Args).


run_proxy(Port, BalancerModule, Options) ->
    log_info("Starting proxy on port ~p with module ~p; options ~p", [Port, BalancerModule, Options]),

    NetModule = case lists:keysearch(certfile, 1, Options) of
        false ->
            gen_tcp;
        {value, _} ->
            log_info("Port ~p will use SSL", [Port]),
            application:start(crypto),
            application:start(public_key),
            case application:start(ssl) of
                {error, {already_started, ssl}} ->
                    no_problem;
                ok ->
                    % Seed SSL if possible.
                    case file:read_file_info("/dev/random") of
                        {error, enoent} ->
                            log_error("Could not seed SSL from /dev/random", []),
                            ssl:seed(term_to_binary(now()));
                        {ok, _} ->
                            ssl:seed(os:cmd("df if=/dev/random bs=256 count=1"))
                    end
            end,
            ssl
    end,

    Restart = fun() ->
        log_info("Restarting proxy", []),
        run_proxy(Port, BalancerModule, Options)
    end,

    Accept = fun(Sock) ->
        %log_info("Accept: ~p", [Sock]),
        case NetModule of
            gen_tcp ->
                gen_tcp:accept(Sock);
            ssl ->
                {ok, SslSock} = ssl:transport_accept(Sock),
                %log_info("ssl:transport_accept done: ~p", [SslSock]),
                case ssl:ssl_accept(SslSock) of
                    {error, Er} ->
                        log_error("Error accepting SSL connection: ~p", [Er]),
                        {error, Er};
                    ok ->
                        %log_info("SSL handshake success", []),
                        {ok, SslSock}
                end
        end
    end

    , log_info("Beginning listen (~p) on ~p by ~p", [NetModule, Port, self()])
    , case NetModule:listen(Port, [binary, {active, false}, {reuseaddr, true}] ++ Options)
        of {ok, ListenSocket}
            -> log_info("Successful listen (~p) on ~p by ~p", [NetModule, Port, self()])
            , tcpAcceptor(ListenSocket, BalancerModule, Accept, Restart)
        ; {error, eaddrinuse}
            -> log_error("Port is already in use: ~p", [Port])
            , ok
        ; Else
            -> log_error("What happened? ~p", [Else])
            , exit(error)
        end
    .


tcpAcceptor(ListeningSocket, BalancerModule, Accept, Restart) ->
    Again = fun() ->
        tcpAcceptor(ListeningSocket, BalancerModule, Accept, Restart)
    end,

    case Accept(ListeningSocket) of
        {ok, Sock} ->
            Pid = spawn(fun () ->
                receive
                    permission ->
                        tcp_setopts(Sock, [ binary
                                          %, {packet, http_bin}
                                          , {packet, http}
                                          , {active, false} ])
                    after 60000 ->
                        timeout
                end,

                collectHttpHeaders(Sock, ?HTTP_HDR_RCV_TMO, BalancerModule, [])
            end),

            tcp_controlling_process(Sock, Pid),
            Pid ! permission,

            Again();
        {error, econnaborted} ->
            log_info("Connection aborted", []),
            Again();
        {error, closed} ->
            log_info("Restarting on closed socket", []),
            Again();
        {error, OtherError} ->
            log_error("Restarting on unexpected error: ~p", [OtherError]),
            Again();
        Msg ->
            error_logger:error_msg("Acceptor died: ~p~n", [Msg]),
            gen_tcp:close(ListeningSocket),
            error_logger:error_msg("Attempting restart", []),
            Restart()
    end.


collectHttpHeaders(Sock, UntilTS, BalancerModule, Headers) ->
  % TODO: Check the timestamp and reduce the timeout for subsequent calls so that the initial
  %       timeout provided is the total timeout of the collect run.
  %Timeout = (UntilTS - tstamp()),
  Timeout = UntilTS,

  tcp_setopts(Sock, [{active, once}]),
  receive
    % Add this next header into the pile of already received headers
    {Type, Sock, {http_header, _Length, Key, undefined, Value}} when Type == ssl orelse Type == http ->
        collectHttpHeaders(Sock, UntilTS, BalancerModule,
                [{header, {Key,Value}} | Headers]);

    {Type, Sock, {http_request, Method, Path, HTTPVersion}} when Type == ssl orelse Type == http ->
        collectHttpHeaders(Sock, UntilTS, BalancerModule,
                [{http_request, Method, Path, HTTPVersion} | Headers]);

    {Type, Sock, http_eoh} when Type == ssl orelse Type == http ->
        % With the headers known, allow the registered proxy module to decide what to do with the query.
        Packets = lists:reverse(Headers),
        case lists:keytake(http_request, 1, Packets) of
            {value, RequestPacket, HeaderPackets} ->
                {http_request, Method, Path, HttpVersion} = RequestPacket,
                RequestHeaders = [{atomify(Key), Val} || {header, {Key, Val}} <- HeaderPackets],
                Request = #request{socket=Sock, method=Method, path=Path, version=HttpVersion, headers=RequestHeaders},

                poison_pill(Request, "preroute"),
                case apply(BalancerModule, route_request, [Request]) of
                    {route, {Host, Port}} ->
                        proxy(Request, Host, Port);
                    {route_new_request, {Host, Port}, NewRequest} ->
                        proxy(NewRequest, Host, Port);
                    {raw_route, {Host, Port}, Data} ->
                        %log_info("raw_route~n~p", [Data]),
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
                        log_info("Unknown ~p:route_request for ~p -> ~p", [BalancerModule, Request, Else]),
                        ok
                end,
                poison_pill(Request, "postroute");
            false ->
                log_error("Failed to parse request: ~p", [Packets])
        end;
    {tcp_closed, Sock} ->
        log_info("Incomplete query on socket: ~p", []),
        tcp_close(Sock);
    {ssl_closed, Sock} ->
        log_info("Incomplete query on SSL socket: ~p", [Headers]),
        tcp_close(Sock);
    Msg ->
        log_info("Invalid message received: ~p~nAfter: ~p~n", [Msg, lists:reverse(Headers)]),
        tcp_close(Sock)

    after Timeout ->
        log_info("timeout; so far: ~p", [Headers]),
        reply(#request{socket=Sock}, {408, "Timeout"}, [{'Content-Type', <<"text/html">>}], "<html><title>Request timeout</title><body><h1>Request timeout</h1></body></html>")
    end.


reply(Request, Status) ->
    reply(Request, Status, []).

reply(Request, Status, Headers) ->
    {_, Message} = Status,
    reply(Request, Status, Headers, list_to_binary(Message)).

reply(Request, Status, Headers, Body) ->
    log_info("reply Status=~p Headers=~p Body=~p", [Status, Headers, Body]),
    poison_pill(Request, "prereply"),

    {StatusCode, StatusMessage} = Status,
    StatusBytes = [
        case Request#request.version of
            {1, 1} -> <<"HTTP/1.1 ">>;
            _      -> <<"HTTP/1.0 ">>
        end,
        list_to_binary(integer_to_list(StatusCode)),
        <<" ">>,
        StatusMessage
    ]

    , HeaderBytes = case(Headers)
        of null -> <<"">> % Headers will be included in the body so don't insert any data.
        ;  _ -> make_headers(lists:keystore('Content-Length', 1, Headers, {'Content-Length', integer_to_list(size(Body))}))
        end
    , tcp_send(Request#request.socket, [StatusBytes, <<"\r\n">>, HeaderBytes, Body])
    , poison_pill(Request, "postreply")
    .

proxy(Req, Ip, Port) ->
    proxy_raw(Req, request_to_binary(Req), Ip, Port).

proxy_raw(Req, Data, Ip, Port) ->
    poison_pill(Req, "preproxy"),
    { ok, ToSocket } = gen_tcp:connect(Ip, Port, [binary, {packet, 0} ]),
    %log_info("Sending, ToSocket = ~p", [ToSocket]),
    tcp_send(ToSocket, Data),
    relay(Req#request.socket, ToSocket, size(Data), 0),
    poison_pill(Req, "postproxy").

make_request(Method, PathTuple, Version) ->
    {abs_path, Path} = PathTuple,

    [if is_list(Method) -> Method;
        true -> atom_to_list(Method)
     end,
     <<" ">>,
     Path,
     case Version of
        {1,1} -> <<" HTTP/1.1\r\n">>;
        _     -> <<" HTTP/1.0\r\n">>
     end].

make_headers(Headers) ->
    ToKey = fun(Key) ->
        if
            is_atom(Key) -> atom_to_list(Key);
            true         -> Key
        end
    end,
    [ [ [ToKey(Key), <<": ">>, Value, <<"\r\n">> ] || {Key, Value} <- Headers ] , <<"\r\n">>].


request_to_binary(Req) ->
    list_to_binary([
        make_request(Req#request.method, Req#request.path, Req#request.version),
        make_headers(Req#request.headers) ]).


relay(FromSocket, ToSocket, BytesIn, BytesOut) ->
    % Note, data from FromSocket increments BytesIn; data from ToSocket increments BytesOut.
    tcp_setopts(FromSocket, [{packet, 0}, {active, once} ]),
    tcp_setopts(ToSocket,   [{packet, 0}, {active, once} ]),
    receive
        {Type, FromSocket, Data} when Type == tcp orelse Type == ssl ->
            %log_info("Data: ~p", [Data]),
            tcp_send(ToSocket, Data),
            relay(FromSocket, ToSocket, BytesIn + size(Data), BytesOut);
        {Type, ToSocket, Data} when Type == tcp orelse Type == ssl ->
            %log_info("Data: ~p", [Data]),
            tcp_send(FromSocket, Data),
            relay(FromSocket, ToSocket, BytesIn, BytesOut + size(Data));
        {tcp_closed, _} ->
            { ok, BytesIn, BytesOut };
        {ssl_closed, _} ->
            { ok, BytesIn, BytesOut };
        Else ->
            log_error("Relay error: ~p", [Else])
    after
        30000 ->
            { error, timeout }
    end.


header(Req, Key)
    -> case lists:keyfind(Key, 1, Req#request.headers)
	of false -> ""
	; {Key, Val} -> Val
	end
    .


%%
%% Helper functions that work on normal or SSL sockets.
%%

socket_type(Socket) ->
    case Socket of
        {sslsocket, _, _} -> ssl;
        _                 -> tcp
    end.

tcp_send(Socket, Data) ->
    case socket_type(Socket) of
        ssl -> ssl:send(Socket, Data);
        tcp -> gen_tcp:send(Socket, Data)
    end.

tcp_close(Socket) ->
    case socket_type(Socket) of
        ssl -> ssl:close(Socket);
        tcp -> gen_tcp:close(Socket)
    end.

tcp_setopts(Socket, Opts) ->
    case socket_type(Socket) of
        ssl -> ssl:setopts(Socket, Opts);
        tcp -> inet:setopts(Socket, Opts)
    end.

tcp_controlling_process(Socket, NewOwner) ->
    case socket_type(Socket) of
        ssl -> ssl:controlling_process(Socket, NewOwner);
        tcp -> gen_tcp:controlling_process(Socket, NewOwner)
    end.


atomify(Val) ->
    case is_binary(Val) of
        true -> list_to_atom(binary_to_list(Val));
        _    -> Val
    end.

% The ability to crash the server with a header for testing.
poison_pill(Request, Hook) ->
    case lists:keyfind("X-Somdune-Die", 1, Request#request.headers) of
        {"X-Somdune-Die", Hook} ->
            log_info("Activating poison pill at ~p", [Hook]),
            exit(poison_pill);
        _ -> ok
    end.


% vim: sts=4 sw=4 et
