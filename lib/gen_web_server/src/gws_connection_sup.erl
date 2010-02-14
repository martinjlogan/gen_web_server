%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2009, Martin Logan
%%% @doc
%%% simple one for one supervisor for handling http connections.
%%% @end
%%% Created : 13 May 2009 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(gws_connection_sup).

-behaviour(supervisor).

%% API
-export([start_link/3, start_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 1156).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link(Callback, Port, UserArgs) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Callback, Port, UserArgs) ->
    error_logger:info_msg("gws_connection_sup:start_link/3 ~p ~p ~p~n",
			  [Callback, Port, UserArgs]),
    {ok, Pid} = supervisor:start_link(?MODULE, [Callback, Port, UserArgs]),
    start_child(Pid),
    {ok, Pid}.

%%--------------------------------------------------------------------
%% @doc
%% Start a child process, an sc_connection.
%%
%% @spec start_child() -> void()
%% @end
%%--------------------------------------------------------------------
start_child(Server) ->
    supervisor:start_child(Server, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%% ignore |
%% {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([Callback, Port, UserArgs]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 1,
    
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    
    Restart = temporary,
    Shutdown = brutal_kill,
    Type = worker,
    
    error_logger:info_msg("Start connection sup with ~p ~p ~p~n", [Port, Callback, UserArgs]),
    {ok, LSock} = gen_tcp:listen(Port, [binary, {active, false}, {packet, raw}, {reuseaddr, true}]),

    WebSocket = {gws_server, {gws_server, start_link, [Callback, LSock, UserArgs]},
		 Restart, Shutdown, Type, [gws_server]},
    
    {ok, {SupFlags, [WebSocket]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
