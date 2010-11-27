%%% Copyright 2010 CouchOne, Inc.
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

%% This is an example Somdune plugin which "swallows" external web sites
%% and places them in subdirectories within this server. For example:
%%
%% localhost:8888/twitter/   -> Proxies to twitter.com
%% localhost:8888/jhs/      -> Proxies to jhs.couchone.com:5984
%% localhost:8888/couchone/ -> Proxies to www.couchone.com

-module(webdirs).
-author('Jason Smith <jhs@couchone.com>').
-version("1").

-include("somdune.hrl").

-define(INFO, fun(Msg, Args) -> error_logger:info_msg(Msg ++ "~n", Args) end).
-define(ERROR, fun(Msg, Args) -> error_logger:error_msg(Msg ++ "~n", Args) end).

-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/0, start_link/0]).

% The Somdune API:
-export([route_request/1]).

% Called by the erl command line `-s webdirs` option.
start() ->
    ok = application:start(somdune),
    application:start(webdirs).

% Called by webdirs_sup
start_link() ->
    ?INFO("starting gen_server:start_link", []),
    gen_server:start_link({local, ?MODULE}, ?MODULE, nil, []).


%%
%% Actual code starts here.
%%

-define(HTTP_PORT, 8888).
-define(WEBDIRS, [{"twitter" , ["twitter.com"     , 80  ]},
                  {"jhs"     , ["jhs.couchone.com", 5984]},
                  {"couchone", ["www.couchone.com", 80  ]}
                 ]).

start_proxy() ->
    % This is called by init/1 and kicks off the HTTP proxy.
    ?INFO("Starting proxy on port: ~p", [?HTTP_PORT]),

    % While any module can register any other module to be a proxy, the
    % simplest way is for this module to register itself. Somdune will
    % spawn a new process which will call into that module's
    % route_request/1 function.
    somdune:register_balancer(?HTTP_PORT, ?MODULE),

    % That's it!
    {ok, null}.


route_request(Request) ->
    ?INFO("Request: ~p", [Request]),
    {reply, {404, "Not implemented"}}.

%%
%% gen_server API
%%

init(Arg) ->
    start_proxy().

handle_call(_Msg, _From, State) ->
    {reply, null, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
    
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    ?INFO("Changing code!", []),
    %% Things to do during code change go here.
    {ok, State}.


% vim: sts=4 sw=4 et
