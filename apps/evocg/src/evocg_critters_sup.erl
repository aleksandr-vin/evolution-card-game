%%%-------------------------------------------------------------------
%%% @author Aleksandr Vinokurov <aleksandr.vin@gmail.com>
%%% @copyright (C) 2012, Aleksandr Vinokurov
%%% @doc
%%% Critters simple_one_for_one supervisor.
%%%
%%% Is created for supervising all the critters of one player.
%%% @end
%%% Created : 19 Oct 2012 by Aleksandr Vinokurov <aleksandr.vin@gmail.com>
%%%-------------------------------------------------------------------
-module(evocg_critters_sup).

-behaviour(supervisor).

%% API
-export([start_link/1, new_critter/1]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(PlayerName) ->
    supervisor:start_link({local, regname(PlayerName)}, ?MODULE, [PlayerName]).

%%--------------------------------------------------------------------
%% @doc
%% Starts a new critter -- evocg_creature gen_fsm
%%
%% @spec new_critter() -> supervisor :: startchild_ret()
%% @end
%%--------------------------------------------------------------------
new_critter(PlayerName) ->
    supervisor:start_child(regname(PlayerName), []).

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
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init(PlayerName) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = transient,
    Shutdown = brutal_kill,
    Type = worker,

    CreatureMod = evocg_creature,

    AChild = {CreatureMod, {CreatureMod, start_link, [PlayerName]},
              Restart, Shutdown, Type, [CreatureMod]},

    {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

regname(PlayerName) ->
    erlang:list_to_atom(PlayerName ++ "'s " ++ ?MODULE_STRING).
