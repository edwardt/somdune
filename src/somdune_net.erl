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
-export([proxy/2]).


info(Msg, Args) -> error_logger:info_msg(Msg, Args).
error(Msg, Args) -> error_logger:error_msg(Msg, Args).


proxy(Port, Module) ->
    info("Starting proxy on port ~p with module ~p", [Port, Module]),
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, false}]),
    tcpAcceptor(null, ListenSocket).


tcpAcceptor(Srv, ListeningSocket) ->
        case gen_tcp:accept(ListeningSocket) of
                {ok, Sock} ->
                        Pid = spawn(fun () ->
                                receive
                                    permission ->
                                        inet:setopts(Sock, [
                                                binary,
                                                {packet, http_bin},
                                                {active, true}
                                        ])
                                    after 60000 -> timeout
                                end,
                                collectHttpHeaders(Srv, Sock, ?HTTP_HDR_RCV_TMO, [])
                        end),
                        gen_tcp:controlling_process(Sock, Pid),
                        Pid ! permission,

                        tcpAcceptor(Srv, ListeningSocket);

                {error, econnaborted} ->
                        tcpAcceptor(Srv, ListeningSocket);
                {error, closed} -> finished;
                Msg ->
                        error_logger:error_msg("Acceptor died: ~p~n", [Msg]),
                        gen_tcp:close(ListeningSocket)
        end.


collectHttpHeaders(Srv, Sock, UntilTS, Headers) ->
  % TODO: Check the timestamp and reduce the timeout for subsequent calls so that the initial
  %       timeout provided is the total timeout of the collect run.
  %Timeout = (UntilTS - tstamp()),
  Timeout = UntilTS,

  receive
    % Add this next header into the pile of already received headers
    {http, Sock, {http_header, _Length, Key, undefined, Value}} ->
        collectHttpHeaders(Srv, Sock, UntilTS,
                [{header, {Key,Value}}|Headers]);

    {http, Sock, {http_request, Method, Path, HTTPVersion}} ->
        collectHttpHeaders(Srv, Sock, UntilTS,
                [{http_request, decode_method(Method), Path, HTTPVersion}
                        | Headers]);

    {http, Sock, http_eoh} ->
        inet:setopts(Sock, [{active, false}, {packet, 0}]),
        reply(Sock, lists:reverse(Headers),
                fun(Hdrs) -> dispatch_http_request(Srv, Hdrs) end);

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


decode_method(Method) ->
    "HI".

reply(Sock, Headers, Callback) ->
    error("I should reply!", []).

dispatch_http_request(Server, Headers) ->
    error("I should dispatch HTTP", []).

% vim: sts=4 sw=4 et
