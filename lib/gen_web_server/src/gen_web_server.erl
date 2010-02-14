%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2010, Martin Logan
%%% @doc
%%%  The main interface for the gen_web_server application
%%% @end
%%% Created : 10 Feb 2010 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(gen_web_server).

%% API
-export([start_link/3, http_reply/1, http_reply/3]).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{init,1},
     {head, 2},
     {get, 2},
     {delete, 2},
     {options, 3},
     {post, 3},
     {put, 3},
     {trace, 3},
     {connect, 3},
     {other_methods, 3}];
behaviour_info(_Other) ->
    undefined.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Start a new gen_web_server behaviour container.
%% @spec (Callback, Port, UserArgs) -> {ok, Pid}
%% @end
%%--------------------------------------------------------------------
start_link(Callback, Port, UserArgs) ->
    gws_connection_sup:start_link(Callback, Port, UserArgs).
    
%%--------------------------------------------------------------------
%% @doc helper function for creating a very minimally specified
%%      http message
%% @spec (Code, Headers, Body) -> ok
%% @end
%%--------------------------------------------------------------------
http_reply(Code, Headers, Body) when is_list(Body) ->
    http_reply(Code, Headers, list_to_binary(Body));
http_reply(Code, Headers, Body) ->
    ["HTTP/1.1 ", code_to_binary(Code), "\r\n",
     format_headers(Headers),
     "Content-Length: ", integer_to_list(size(Body)), 
     "\r\n\r\n",
     Body].

%% @spec (Code) -> ok
%% @equiv http_reply(Code, [{"Content-Type", "text/html"}], "") -> ok
http_reply(Code) ->
    http_reply(Code, [{"Content-Type", "text/html"}], "").

format_headers([{Header, Value}|T]) ->
    [Header, ": ", Value, "\r\n"|format_headers(T)];
format_headers([]) ->
    [].

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc Given a number of a standard HTTP response code, return
%% a binary (string) of the number and name.
%%
%% Example:
%% ```code_to_binary(404) => <<"404 Not Found">>
%% '''
%%
%% The supported status codes are taken from:
%%   ["http://en.wikipedia.org/wiki/List_of_HTTP_status_codes"]
%%
%% @spec (integer()) -> binary()
code_to_binary(100) -> <<"100 Continue">>;
code_to_binary(101) -> <<"101 Switching Protocols">>;
code_to_binary(102) -> <<"102 Processing">>;
code_to_binary(200) -> <<"200 OK">>;
code_to_binary(201) -> <<"201 Created">>;
code_to_binary(202) -> <<"202 Accepted">>;
code_to_binary(203) -> <<"203 Non-Authoritative Information">>;
code_to_binary(204) -> <<"204 No Content">>;
code_to_binary(205) -> <<"205 Reset Content">>;
code_to_binary(206) -> <<"206 Partial Content">>;
code_to_binary(207) -> <<"207 Multi-Status">>;
code_to_binary(300) -> <<"300 Multiple Choices">>;
code_to_binary(301) -> <<"301 Moved Permanently">>;
code_to_binary(302) -> <<"302 Found">>;
code_to_binary(303) -> <<"303 See Other">>;
code_to_binary(304) -> <<"304 Not Modified">>;
code_to_binary(305) -> <<"305 Use Proxy">>;
code_to_binary(307) -> <<"307 Temporary Redirect">>;
code_to_binary(400) -> <<"400 Bad Request">>;
code_to_binary(401) -> <<"401 Unauthorized">>;
code_to_binary(402) -> <<"402 Payment Required">>;
code_to_binary(403) -> <<"403 Forbidden">>;
code_to_binary(404) -> <<"404 Not Found">>;
code_to_binary(405) -> <<"405 Method Not Allowed">>;
code_to_binary(406) -> <<"406 Not Acceptable">>;
code_to_binary(407) -> <<"407 Proxy Authentication Required">>;
code_to_binary(408) -> <<"408 Request Time-out">>;
code_to_binary(409) -> <<"409 Conflict">>;
code_to_binary(410) -> <<"410 Gone">>;
code_to_binary(411) -> <<"411 Length Required">>;
code_to_binary(412) -> <<"412 Precondition Failed">>;
code_to_binary(413) -> <<"413 Request Entity Too Large">>;
code_to_binary(414) -> <<"414 Request-URI Too Large">>;
code_to_binary(415) -> <<"415 Unsupported Media Type">>;
code_to_binary(416) -> <<"416 Requested range not satisfiable">>;
code_to_binary(417) -> <<"417 Expectation Failed">>;
code_to_binary(421) ->
    <<"421 There are too many connections from your internet address">>;
code_to_binary(422) -> <<"422 Unprocessable Entity">>;
code_to_binary(423) -> <<"423 Locked">>;
code_to_binary(424) -> <<"424 Failed Dependency">>;
code_to_binary(425) -> <<"425 Unordered Collection">>;
code_to_binary(426) -> <<"426 Upgrade Required">>;
code_to_binary(449) -> <<"449 Retry With">>;
code_to_binary(500) -> <<"500 Internal Server Error">>;
code_to_binary(501) -> <<"501 Not Implemented">>;
code_to_binary(502) -> <<"502 Bad Gateway">>;
code_to_binary(503) -> <<"503 Service Unavailable">>;
code_to_binary(504) -> <<"504 Gateway Time-out">>;
code_to_binary(505) -> <<"505 HTTP Version not supported">>;
code_to_binary(506) -> <<"506 Variant Also Negotiates">>;
code_to_binary(507) -> <<"507 Insufficient Storage">>;
code_to_binary(509) -> <<"509 Bandwidth Limit Exceeded">>;
code_to_binary(510) -> <<"510 Not Extended">>;
code_to_binary(Code) -> Code.
