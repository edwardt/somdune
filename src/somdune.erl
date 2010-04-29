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


-module(somdune).
-author('Jason Smith <jhs@couch.io>').

-behaviour(application).
-export([start/2, stop/1]).

-include("somdune.hrl").
-export([start/0, version/0]).

% XXX Development stuff
-export([t/0, route_request/1]).

%
% The user-level API
%

-export([register_balancer/2]).

register_balancer(Port, Module) ->
    gen_server:call(?MANAGER, {register, Port, Module}).


%% --------------------------------------------------------------------
%% Generic utilities.
%% --------------------------------------------------------------------

%% @spec () -> ok
%% @doc Start applications which exmpp depends on then start exmpp.
start() ->
    application:start(somdune).  

%
% application API
%

start(_Start_Type, _Start_Args) ->
    case start_apps([]) of
        ok->
            somdune_sup:start_link();
        {error, Reason} ->
            {error, Reason}
    end.
    

stop(_State) ->
    ok.
    

version() ->
    {ok, Version} = application:get_key(somdune, vsn),
    Version.   


% A simple way to start apps which this application depends on.
start_apps([]) ->
    ok;

start_apps([App|Rest]) ->
    case application:start(App) of
    ok ->
       start_apps(Rest);
    {error, {already_started, App}} ->
       start_apps(Rest);
    {error, _Reason} ->
       {error, {app_would_not_start, App}}
    end.


% XXX: Development hooks.
t() ->
    somdune:start(),
    somdune:register_balancer(15985, somdune).

route_request(Request) ->
    error_logger:info_msg("Got request: ~p~n", [Request]),
    {route, {"localhost", 5985}}.

% vim: sts=4 sw=4 et
