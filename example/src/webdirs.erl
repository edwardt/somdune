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
-define(WEBDIRS, [ {"couchone", ["www.couchone.com", 80  ]}
                 , {"jhs"     , ["jhs.couchone.com", 5984]}
                 , {"twitter" , ["twitter.com"     , 80  ]}
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
    {abs_path, Path} = Request#request.path,
    ?INFO("Request for path: ~p", [Path]),

    case Path of
        <<"/reload">> ->
            ?INFO("Reloading plugin", []),
            _Pid = spawn(fun() -> reload() end),
            {reply, {200, "Reloading"}, [{'Content-Type', <<"text/html">>}], <<"Policy reloaded. <a href='/'>Try now.</a>\r\n">>};
        _ ->
            Dirs = string:tokens(binary_to_list(Path), "/"),
            case Dirs of
                [] ->
                    % User queried /
                    default_reply();
                _ ->
                    % User queried /something (or /something/foo, whatever).
                    TopLevelDir = lists:nth(1, Dirs),
                    case proplists:get_value(TopLevelDir, ?WEBDIRS) of
                        undefined ->
                            % Unknown subdirectory, no big deal.
                            default_reply();
                        [BackendHost, BackendPort] ->
                            % Send them through! It would be nice to simply return % {route, {BackendHost, BackendPort}}.
                            % That can be okay with hostname-based virtual hosting; however these servers will not like
                            % the strange "Host" header, as well as the strange path (/couchone, etc.).
                            % Therefore use Somdune's support to modify the request before sending it through.

                            % Strip the first directory, so the remote server will not see /foo/bar but just /bar.
                            NewPath = case lists:nthtail(1, Dirs) of
                                []       -> "/";
                                SubDirs  -> "/" ++ filename:join(SubDirs)
                            end,

                            % Correct the Host header.
                            FixedHost = lists:keystore('Host', 1, Request#request.headers, {'Host', list_to_binary(BackendHost)}),

                            % Always add a Connection: close since Somdune becomes a raw TCP proxy after the route is established.
                            % Since every request needs its Host header changed, using "Connection: close" will force the browser
                            % to reconnect each time.
                            NewHeaders = lists:keystore('Connection', 1, FixedHost, {'Connection', <<"close">>}),

                            NewRequest = Request#request{path={abs_path, NewPath}, headers=NewHeaders},
                            {route_new_request, {BackendHost, BackendPort}, NewRequest}
                    end
            end
    end.


default_reply() ->
    % A simple HTML reply for unrecognized paths, or the root URL, or whatever.
    Status = {200, "Default reply"},
    Headers = [{'Content-Type', <<"text/html">>}],
    Body = iolist_to_binary([ "<html><head><title>Webdirs</title></head><body>"
                            , "<h1>Webdirs</h1>"
                            , "These web sites are hosted inside this web site:"
                            , "<ul>"
                            , lists:map(fun(Prop) ->
                                            {Key, [Host, Port]} = Prop,
                                            ["<li>", "<a href='/", Key, "/'>", Key, "</a> is ", Host, ":", integer_to_list(Port), "</li>"]
                                        end, ?WEBDIRS)
                            , "<li><a href='/reload'>Reload the routing policy</a></li>"
                            , "</ul>"
                            , "</body></html>"
                            ]),
    {reply, Status, Headers, Body}.


reload() ->
    ?INFO("I should reload: ~p\n", [?MODULE]),
    {ok, ?MODULE} = compile:file('src/webdirs', [
        verbose, report_errors, report_warnings,
        {i, "../include"},
        {i, "./include"},
        {outdir, "ebin"}
    ]),
    ok = sys:suspend(?MODULE),
    case code:purge(?MODULE) of
        true -> ok;
        false -> ?INFO("code:purge returned false; no big deal", [])
    end,
    {module, ?MODULE} = code:load_file(?MODULE),
    ok = sys:change_code(?MODULE, ?MODULE, "1", []),
    ok = sys:resume(?MODULE).

%%
%% gen_server API
%%

init(_Arg) ->
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
