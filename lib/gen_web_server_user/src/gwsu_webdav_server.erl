%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2010, Martin Logan
%%% @doc
%%%  gen web server implementation
%%% @end
%%% Created : 11 Feb 2010 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(gwsu_webdav_server).

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
    gen_web_server:start_link(?MODULE, 8090, "/tmp/repo/").

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
%% @spec (RequestLine, Headers, State) -> Response
%% @end
%%--------------------------------------------------------------------
get({http_request, _, {abs_path, AbsPathBin}, _}, Headers, State) ->
    AbsPath = binary_to_list(AbsPathBin),
    FilePath = filename:join(State#state.document_root, string:strip(AbsPath, left, $\/)),
    case catch file:read_file(FilePath) of
	{ok, TarFile} ->
	    error_logger:info_msg("request is ~p~n", [FilePath]),
	    gen_web_server:http_reply(200, Headers, TarFile);
	_Error ->
	    gen_web_server:http_reply(404)
    end.
	    
head(_RequestLine, _Headers, _State) -> gen_web_server:http_reply(200).
delete(_RequestLine, _Headers, _State) -> gen_web_server:http_reply(200).

%%--------------------------------------------------------------------
%% @doc
%% @spec (RequestLine, Headers, Body, State) -> Response
%% @end
%%--------------------------------------------------------------------
put({http_request, _, {abs_path, AbsPathBin}, _}, _Headers, Body, State) ->
    AbsPath = binary_to_list(AbsPathBin),
    To = filename:join(State#state.document_root, string:strip(AbsPath, left, $\/)),
    case catch write_data(Body, To) of
	ok ->
	    gen_web_server:http_reply(201);
	_ ->
	    gen_web_server:http_reply(405)
    end.
trace(_RequestLine, _Headers, _Body, _State) -> gen_web_server:http_reply(200).
post(_RequestLine, _Headers, _Body, _State) -> gen_web_server:http_reply(200).
options(_RequestLine, _Headers, _Body, _State) -> gen_web_server:http_reply(200).

%%--------------------------------------------------------------------
%% @doc
%% @spec (RequestLine, Headers, Body) -> Response
%% @end
%%--------------------------------------------------------------------
other_methods({http_request, <<"PROPFIND">>, {abs_path, AbsPathBin}, _}, Headers, _Body, State) ->
    AbsPath = binary_to_list(AbsPathBin),
    {value, {'Host', Host}} = lists:keysearch('Host', 1, Headers),
    case gws_web_dav_util:propfind(State#state.document_root, AbsPath, binary_to_list(Host), 1) of
	error -> 
	    gen_web_server:http_reply(404);
	Resp -> 
	    error_logger:info_msg("request is ~p ~p~n", [AbsPath, Headers]),
	    WebResp = gen_web_server:http_reply(207, Headers, Resp),
	    error_logger:info_msg("response to propfind ~p~n", [WebResp]),
	    WebResp
    end;
other_methods({http_request, <<"MKCOL">>, {abs_path, AbsPathBin}, _}, _Headers, _Body, State) ->
    AbsPath = binary_to_list(AbsPathBin),
    gws_web_dav_util:mkcol(State#state.document_root, AbsPath),
    gen_web_server:http_reply(201);
other_methods(RequestLine, Headers, Body, _State) ->
    error_logger:info_msg("request is ~p ~p ~p~n", [RequestLine, Headers, Body]),
    gen_web_server:http_reply(200).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%-------------------------------------------------------------------
%% @spec write_data(Data, Location) -> ok | {error, Reason}
%% @doc
%%  Write the data to the specified location.
%% @end
%% @private
%%-------------------------------------------------------------------
write_data(Data, To) ->
    case file:open(To, [write, raw]) of
        {ok, Fd} ->
	    error_logger:info_msg("ewr_fetch:write_data writing to ~p~n", [To]),
            ok = file:write(Fd, Data),
            file:close(Fd),
            ok;
        {error, Reason} ->
            throw({file_open_error, Reason})
    end.
