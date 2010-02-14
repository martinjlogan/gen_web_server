%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2010, Martin Logan
%%% @doc
%%%  gen web server implementation
%%% @end
%%% Created : 11 Feb 2010 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(gwsu_web_server).

-behaviour(gen_web_server).

%% API
-export([start_link/0]).

%% callbacks
-export([
	 init/1,
	 head/2,
	 get/2,
	 delete/2,
	 options/3,
	 post/3,
	 put/3,
	 trace/3,
	 connect/3,
	 other_methods/3
	]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_web_server:start_link(?MODULE, 8080, []).

%%%===================================================================
%%% gen_web_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc 
%% @spec (UserArgs) -> void()
%% @end
%%--------------------------------------------------------------------
init(UserArgs) ->
    {ok, UserArgs}.

%%--------------------------------------------------------------------
%% @doc
%% @spec (RequestLine, Headers) -> Response
%% @end
%%--------------------------------------------------------------------
get(RequestLine, Headers) ->
    Body = "<h2>Welcome to the gen_web_server</h2>"
           "<p>Docs can be found at erlware.org or by"
           " generating edocs on the app</p>",
    gen_web_server:http_reply(200, Headers, Body).

head(_RequestLine, _Headers) -> gen_web_server:http_reply(200).
delete(_RequestLine, _Headers) -> gen_web_server:http_reply(200).

%%--------------------------------------------------------------------
%% @doc
%% @spec (RequestLine, Headers, Body) -> Response
%% @end
%%--------------------------------------------------------------------
put(RequestLine, Headers, Body) ->
    gen_web_server:http_reply(200).

trace(_RequestLine, _Headers, _Body) -> gen_web_server:http_reply(200).
post(_RequestLine, _Headers, _Body) -> gen_web_server:http_reply(200).
options(_RequestLine, _Headers, _Body) -> gen_web_server:http_reply(200).
connect(_RequestLine, _Headers, _Body) -> gen_web_server:http_reply(200).

%%--------------------------------------------------------------------
%% @doc
%% @spec (RequestLine, Headers, Body) -> Response
%% @end
%%--------------------------------------------------------------------
other_methods(RequestLine, Headers, Body) ->
    error_logger:info_msg("request is ~p ~p ~p~n", [RequestLine, Headers, Body]),
    gen_web_server:http_reply(200).

%%%===================================================================
%%% Internal functions
%%%===================================================================
