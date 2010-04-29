%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh-2.local>
%%% @copyright (C) 2009, Martin Logan
%%% @doc
%%%  Handle a socket connection for incomming http packets. 
%%% @end
%%% Created : 10 Sep 2009 by Martin Logan <martinjlogan@Macintosh-2.local>
%%%-------------------------------------------------------------------
-module(gws_server).

%% API
-export([start_link/3]).

-record(state, {lsock, socket, request_line, headers = [], body = <<>>,
		content_remaining = 0, callback, user_state, parent,
	        connection}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(Callback, LSock, UserArgs) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Callback, LSock, UserArgs) ->
    % Not using proc lib for efficiency concerns
    Self = self(),
    {ok, spawn_link(fun() -> init(Callback, LSock, UserArgs, Self) end)}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

init(Callback, LSock, UserArgs, Parent) ->
    {ok, UserState} = Callback:init(UserArgs),
    accept(#state{lsock = LSock, callback = Callback, user_state = UserState, parent = Parent}).

accept(#state{lsock = LSock, parent = Parent} = State) ->
    {ok, Socket} = gen_tcp:accept(LSock),
    gws_connection_sup:start_child(Parent),
    inet:setopts(Socket,[{active,once}]),
    collect_request_line(State#state{socket = Socket}).

collect_request_line(State) ->
    receive
	{http, _Socket, {http_request, _Method, _Path, {1,1}} = RequestLine} ->
	    inet:setopts(State#state.socket, [{active,once}]),
	    collect_headers(State#state{request_line = RequestLine, connection = persistent});
	{http, _Socket, {http_request, _Method, _Path, _HTTPVersion} = RequestLine} ->
	    inet:setopts(State#state.socket, [{active,once}]),
	    collect_headers(State#state{request_line = RequestLine, connection = close});
	{tcp_closed, _Socket} ->
	    ok
    end.

collect_headers(#state{headers = Headers} = State) ->
    receive
	{http, _Socket, {http_header, _Length, 'Connection', _, <<"close">>}} ->
	    inet:setopts(State#state.socket, [{active,once}]),
	    collect_headers(State#state{headers = [{'Connection', <<"close">>}|Headers], connection = close});
	{http, _Socket, {http_header, _Length, Value, _, <<"100-continue">>}} ->
	    gen_tcp:send(State#state.socket, gen_web_server:http_reply(100)),
	    inet:setopts(State#state.socket, [{active,once}]),
	    collect_headers(State#state{headers = [{Value, <<"100-continue">>}|Headers]});
	{http, _Socket, {http_header, _Length, 'Content-Length', _, Value}} ->
	    ContentRemaining = list_to_integer(binary_to_list(Value)),
	    inet:setopts(State#state.socket, [{active,once}]),
	    collect_headers(State#state{headers = [{'Content-Length', Value}|Headers], content_remaining = ContentRemaining});
	{http, _Socket, {http_header, _Length, Key, _, Value}} ->
	    inet:setopts(State#state.socket, [{active,once}]),
	    collect_headers(State#state{headers = [{Key, Value}|Headers]});
	{http, _Socket, http_eoh} when State#state.content_remaining == 0, State#state.connection == persistent ->
	    Reply = callback(State),
	    gen_tcp:send(State#state.socket, Reply),
	    NewState = State#state{request_line = undefined, headers = [], body = <<>>},
	    collect_request_line(NewState);
	{http, _Socket, http_eoh} when State#state.content_remaining == 0 ->
	    Reply = callback(State),
	    gen_tcp:send(State#state.socket, Reply);
	{http, _Socket, http_eoh} ->
	    inet:setopts(State#state.socket, [{active,once}, {packet, raw}]),
	    collect_body(State);
	{tcp_closed, _Socket} ->
	    ok
    end.

collect_body(State) ->
    receive
	{tcp, _Socket, Packet} ->
	    PacketSize       = byte_size(Packet),
	    ContentRemaining = State#state.content_remaining - PacketSize,
	    Body             = list_to_binary([State#state.body, Packet]),
	    NewState = State#state{body = Body, content_remaining = ContentRemaining},
	    case ContentRemaining of
		0 when NewState#state.connection == persistent ->
		    Reply = callback(NewState),
		    gen_tcp:send(State#state.socket, Reply),
		    FreshState = NewState#state{request_line = undefined, headers = [], body = <<>>},
		    collect_request_line(FreshState);
		0 ->
		    Reply = callback(NewState),
		    gen_tcp:send(State#state.socket, Reply);
		ContentLeftOver when ContentLeftOver > 0 ->
		    inet:setopts(State#state.socket, [{active,once}]),
		    collect_body(NewState)
	    end;
	{tcp_closed, _Socket} ->
	    ok
    end.

callback(State) -> 
    #state{callback     = Callback,
	   request_line = RequestLine,
	   headers      = Headers,
	   body         = Body,
	   user_state   = UserState} = State,
    handle_message(RequestLine, Headers, Body, Callback, UserState).

handle_message({http_request, 'GET', _, _} = RequestLine, Headers, _Body, CallBack, UserState) ->
    CallBack:get(RequestLine, Headers, UserState);
handle_message({http_request, 'DELETE', _, _} = RequestLine, Headers, _Body, CallBack, UserState) ->
    CallBack:delete(RequestLine, Headers, UserState);
handle_message({http_request, 'HEAD', _, _} = RequestLine, Headers, _Body, CallBack, UserState) ->
    CallBack:head(RequestLine, Headers, UserState);

handle_message({http_request, 'POST', _, _} = RequestLine, Headers, Body, CallBack, UserState) ->
    CallBack:post(RequestLine, Headers, Body, UserState);
handle_message({http_request,'PUT',_,_} = RequestLine, Headers, Body, CallBack, UserState) ->
    CallBack:put(RequestLine, Headers, Body, UserState);
handle_message({http_request, 'TRACE', _, _} = RequestLine, Headers, Body, CallBack, UserState) ->
    CallBack:head(RequestLine, Headers, Body, UserState);
handle_message({http_request, 'OPTIONS', _, _} = RequestLine, Headers, Body, CallBack, UserState) ->
    CallBack:options(RequestLine, Headers, Body, UserState);
handle_message(RequestLine, Headers, Body, CallBack, UserState) ->
    CallBack:other_methods(RequestLine, Headers, Body, UserState).

