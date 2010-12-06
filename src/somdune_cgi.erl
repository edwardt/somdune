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

%% Dead-simple CGI environment variables based on a request.

-module(somdune_cgi).
-author('Jason Smith <jhs@couchone.com>').

-include("somdune.hrl").
-export([prep_request/1]).


prep_request(Request)
    -> {ok, {env_for_request(Request), data_for_request(Request)}}
    .


env_for_request(Req)
    % Clean up the various types in the structure.
    -> [{K, to_list(V)} || {K, V} <- env_for_request(Req, raw)]
    .

env_for_request(Req, raw)
    -> Method = case Req#request.method
	of X when is_list(X) -> X
	;  X when is_atom(X) -> atom_to_list(X)
	end
    , {abs_path, Location} = Req#request.path
    , [FullPath | AfterQMark] = re:split(Location, "\\?")
    , [ {"REQUEST_METHOD"   , Method}
      , {"GATEWAY_INTERFACE", "CGI/1.1"}
      , {"PATH_TRANSLATED"  , FullPath}
      , {"QUERY_STRING"     , binary_to_list(erlang:iolist_to_binary(AfterQMark))}
      , {"CONTENT_LENGTH"   , header(Req, 'Content-Length')}
      ]
    .


data_for_request(Req)
    -> case header(Req, 'Content-Length')
	of ""
	    -> <<>>
	; LenStr
	    -> case header(Req, "Expect") % XXX String, not atom, not sure how future-proof that is.
		of "100-continue"
		    -> somdune_net:tcp_send(Req#request.socket, <<"HTTP/1.1 100 Continue\r\n\r\n">>)
		; _ -> ok
		end
	    , data_for_request(Req#request.socket, [], list_to_integer(LenStr))
	end
    .

data_for_request(Sock, Chunks, Remaining)
    %-> somdune_net:log_info("data_for_request ~p: ~p\n", [Remaining, Chunks])
    -> case Remaining
	of 0
	    %-> somdune_net:log_info("Returning reversal of: ~p\n", [Chunks])
	    -> erlang:iolist_to_binary(lists:reverse(Chunks))
	; _
	    -> somdune_net:tcp_setopts(Sock, [{packet, raw}, {active, once}])
	    , receive
		{Type, Sock, Data} when Type == tcp orelse Type == ssl
		    -> data_for_request(sock, [Data | Chunks], Remaining - size(Data))
		; {Closed, _} when Closed == tcp_closed orelse Closed == ssl_closed
		    -> data_for_request(sock, Chunks, 0)
		; Else
		    %-> somdune_net:log_info("WTF! ~p\n", [Else])
		    -> exit({cgi_data_collection_error, Else})
		end
	end
    .


header(Req, Key)
    -> case lists:keyfind(Key, 1, Req#request.headers)
	of false -> ""
	; {Key, Val} -> Val
	end
    .


to_list(Val)
    -> case Val
	of _ when is_atom(Val) -> atom_to_list(Val)
	;  _ when is_binary(Val) -> binary_to_list(Val)
	;  _ when is_list(Val) -> binary_to_list(erlang:iolist_to_binary(Val))
	end
    .

% vim: sts=4 sw=4 noet
