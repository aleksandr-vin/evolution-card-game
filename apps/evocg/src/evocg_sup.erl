
-module(evocg_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, new_player/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%--------------------------------------------------------------------
%% @doc
%% Starts a new player supervisor
%%
%% It's only evocg_critters_sup for now.
%%
%% TODO: All new OTP servers related to one player must be started
%% under specially created evocg_player_sup.
%%
%% @spec new_player(Name :: string()) -> supervisor :: startchild_ret()
%% @end
%%--------------------------------------------------------------------
new_player(Name) ->
    Restart = transient,
    Shutdown = infinity,
    Type = supervisor,

    Mod = evocg_critters_sup,
    Id = Name ++ "'s " ++ erlang:atom_to_list(Mod),

    CrittersSup = {Id, {Mod, start_link, [Name]},
                   Restart, Shutdown, Type, [Mod]},
    supervisor:start_child(?SERVER, CrittersSup).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.
