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


-module(somdune).
-author('Jason Smith <jhs@couchone.com>').

-include("somdune.hrl").

% XXX Development stuff
-export([start/0, version/0]).
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
    application:start(somdune_app).


version() ->
    {ok, Version} = application:get_key(somdune, vsn),
    Version.


% XXX: Development hooks.
t() ->
    somdune:start(),
    somdune:register_balancer(15985, somdune).

route_request(Request) ->
    error_logger:info_msg("Got request: ~p~n", [Request]),
    {route, {"localhost", 5985}}.

% vim: sts=4 sw=4 et
