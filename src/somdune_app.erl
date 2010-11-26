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


-module(somdune_app).
-author('Jason Smith <jhs@couchone.com>').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case start_apps([]) of
        ok->
            somdune_sup:start_link();
        {error, Reason} ->
            {error, Reason}
    end.


stop(_State) ->
    ok.
    

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

% vim: sts=4 sw=4 et
