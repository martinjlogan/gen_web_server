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
	 head/3,
	 get/3,
	 delete/3,
	 options/4,
	 post/4,
	 put/4,
	 trace/4,
	 connect/4,
	 other_methods/4
	]).

-record(state, {document_root}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_web_server:start_link(?MODULE, 8080, "/tmp/repo/").

%%%===================================================================
%%% gen_web_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc 
%% @spec (UserArgs) -> void()
%% @end
%%--------------------------------------------------------------------
init(DocumentRoot) ->
    {ok, #state{document_root = DocumentRoot}}.

%%--------------------------------------------------------------------
%% @doc
%% @spec (RequestLine, Headers) -> Response
%% @end
%%--------------------------------------------------------------------
get(_RequestLine, Headers, _State) ->
    Body = "<h2>Welcome to the gen_web_server</h2>"
           "<p>Docs can be found at erlware.org or by"
           " generating edocs on the app</p>",
    gen_web_server:http_reply(200, Headers, Body).

head(_RequestLine, _Headers, _State) -> gen_web_server:http_reply(200).
delete(_RequestLine, _Headers, _State) -> gen_web_server:http_reply(200).

%%--------------------------------------------------------------------
%% @doc
%% @spec (RequestLine, Headers, Body, State) -> Response
%% @end
%%--------------------------------------------------------------------
put(_RequestLine, _Headers, _Body, _State) -> gen_web_server:http_reply(200).
trace(_RequestLine, _Headers, _Body, _State) -> gen_web_server:http_reply(200).
post(_RequestLine, _Headers, _Body, _State) -> gen_web_server:http_reply(200).
options(_RequestLine, _Headers, _Body, _State) -> gen_web_server:http_reply(200).
connect(_RequestLine, _Headers, _Body, _State) -> gen_web_server:http_reply(200).
other_methods(_RequestLine, _Headers, _Body, _State) -> gen_web_server:http_reply(200).

%%%===================================================================
%%% Internal functions
%%%===================================================================
