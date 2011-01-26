%%% Copyright 2009 CouchOne, Inc.
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


-module(somdune_sup).
-author('Jason Smith <jhs@couchone.com>').
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% helper macro for declaring children of supervisor
-define(child(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [i]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    SupervisorSpec = {one_for_one, 10, 3600},
    ManagerSpec = ?child(somdune_manager, supervisor),
    {ok, {SupervisorSpec, [ManagerSpec]}}.

% vim: sw=4 sts=4 et
