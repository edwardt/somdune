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


-module(somdune_manager).
-author('Jason Smith <jhs@couchone.com>').

-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("somdune.hrl").
-export([start_link/0]).

%
% The user-level API
%

handle_call({register, Port, Module, Options}, _From, State) ->
    spawn(somdune_net, run_proxy, [Port, Module, Options]),
    {reply, ok, State}.


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, nil, []).

%%
%% gen_server API
%%

init(_) ->
    %process_flag(priority, high),
    {ok, null}.


handle_cast(_Msg, State) ->
    {noreply, State}.
    
handle_info(_Msg, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% vim: sw=4 sts=4 et
