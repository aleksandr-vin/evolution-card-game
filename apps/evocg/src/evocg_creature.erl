%%%-------------------------------------------------------------------
%%% @author Aleksandr Vinokurov <aleksandr.vin@gmail.com>
%%% @copyright (C) 2012, Aleksandr Vinokurov 
%%% @doc
%%% Creature FSM.
%%%
%%% For now it is only one allowed to be created on a node.
%%%
%%% TODO: Add more than one creatures on one node.
%%%
%%% The creature is created in simple (evoluting) state without
%%% properties. Then it can acquire properties changing states to predator for
%%% example or start starving (it's a specified state).
%%%
%%% In starving state the creature can be fed (and change the state to fed in
%%% case), eaten or it can end its turn diying if still in starving
%%% state. Otherwise it return in evolution (or common) state if it was
%%% successfuly fed.
%%%
%%% TODO: Add honors to get_fqn/1, on other creatures kills and so on.
%%%
%%% TODO: Add age to get_fqn/1 or somewhere else.
%%%
%%% @end
%%% Created : 10 Oct 2012 by Aleksandr Vinokurov <aleksandr.vin@gmail.com>
%%%-------------------------------------------------------------------
-module(evocg_creature).

-behaviour(gen_fsm).

-include("log.hrl").

%% API
-export([start_link/0,
         name/1,
         property/1, carnivorous/0, period/0]).

%% gen_fsm callbacks
-export([init/1,
         simple/3, carnivorous/3,
         handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(SERVER, ?MODULE).

-record(state, {attrs = [],
                props = sets:new(),
                carnivorous = false,
                evo_state}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Names a creature (not FSM).
%%
%% @spec name(String :: string()) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
name(String) when is_list(String) ->
    gen_fsm:sync_send_all_state_event(?SERVER, {name, String}).

%%--------------------------------------------------------------------
%% @doc
%% Sets a property to the creature.
%%
%% @spec property(PropName :: string()) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
property(PropName = P) when "норное" == P orelse
                            "большое" == P orelse
                            "отбрасывание хвоста" == P ->
    gen_fsm:sync_send_event(?SERVER, {property, PropName}).

%%--------------------------------------------------------------------
%% @doc
%% Mutates a creature to a carnivorous.
%%
%% @spec carnivorous() -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
carnivorous() ->
    gen_fsm:sync_send_event(?SERVER, carnivorous).

%%--------------------------------------------------------------------
%% @doc
%% Switch periods in creatures' life.
%%
%% Evolution period is changed to starvation period, when the creature must be
%% fed or it die at the end of the period.
%%
%% Starvation period is changed to evolution period, when the creature can
%% acquire properties, mutate and wait for period change.
%%
%% @spec period() -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
period() ->
    gen_fsm:sync_send_all_state_event(?SERVER, period).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    ?info("Simple creature is born."),
    {ok, simple, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------


%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
simple({property, PropName}, _From, State0) ->
    {Reply, State1} = set_property(PropName, State0),
    {reply, Reply, simple, State1};
simple(carnivorous, _From, #state{carnivorous = false} = State0) ->
    ?info("~s became a carnivorous.", [get_name(State0)]),
    Reply = ok,
    {reply, Reply, carnivorous, State0#state{carnivorous = true}}.

carnivorous({property, PropName}, _From, State0) ->
    {Reply, State1} = set_property(PropName, State0),
    {reply, Reply, carnivorous, State1}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event({name, String}, _From, StateName, State0) ->
    {Reply, State1} = set_name(String, State0),
    {reply, Reply, StateName, State1};
handle_sync_event(period, _From, StateName, State0) ->
    case is_evolution(StateName) of
        true ->
            {reply, ok, starvation, State0#state{evo_state = StateName}};
        false ->
            switch_to_evolution(StateName, State0)
    end;
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = {error, 'not-supported'},
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_name(#state{attrs = Attrs}) ->
    case proplists:get_value(name, Attrs) of
        undefined ->
            "Creature";
        Val -> Val
    end.

set_name(String, #state{attrs = Attrs} = State0) ->
    ?info("~s acquire new name ~s.", [get_fqn(State0), String]),
    {ok, State0#state{attrs = [{name, String} | Attrs]}}.

get_char(#state{carnivorous = true}) ->
    "Carnivorous";
get_char(_) ->
    "".

get_fully_qualified_name(State) ->
    lists:flatten([case get_char(State) of
                       undefined -> [];
                       V -> [V, " "]
                   end,
                   get_name(State)]).

get_fqn(State) ->
    get_fully_qualified_name(State).

set_property(Name, #state{props = Props0} = State0) ->
    case sets:is_element(Name, Props0) of
        true ->
            ?error("~s already have property \"~s\".", [get_fqn(State0), Name]),
            {{error, 'already-have-property'}, State0};
        false ->
            ?info("~s acquire property \"~s\".", [get_fqn(State0), Name]),
            {ok, State0#state{props = sets:add_element(Name, Props0)}}
    end.

switch_to_evolution(_StateName, #state{evo_state = EvoState} = State) ->
    {reply, ok, EvoState, State}.

is_evolution(simple) -> true;
is_evolution(carnivorous) -> true;
is_evolution(_) -> false.
