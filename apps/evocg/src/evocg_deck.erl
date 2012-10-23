%% @author Maximus
%% @doc
%% Management of a deck of cards


-module(evocg_deck).
-behaviour(gen_server).

-include("../include/cards.hrl").
-include_lib("eunit/include/eunit.hrl").

% behavior functions
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%api functions
-export([start_link/0, prepare_for_new_game/1]).



-record(state, {deck, players}).





%% ====================================================================
%% API functions
%% ====================================================================


%%
%% @doc
%% Start card deck process
%%
start_link() ->
	gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

%%
%% @doc
%% Prepare for new game
%% Players - number of players
%%
-spec prepare_for_new_game(Players::integer()) -> ok.
prepare_for_new_game(Players) ->
	gen_server:cast(?MODULE, {new_game, Players}).
%% ====================================================================
%% Behavioural functions 
%% ====================================================================

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([]) ->
    {ok, #state{}}.



%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast({new_game, Players}, State) ->
	ShD = shuffle_deck(get_init_set()),
	{noreply, State#state{players=Players,deck=ShD}}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(Reason, State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(OldVsn, State, Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================



%%
%% @doc
%% @private
%% Shuffling card deck
shuffle_deck(Deck) ->
	random:seed(now()),
	{_,Y} = 
		lists:unzip(
		  lists:keysort(1, 
						[{random:uniform(100000),X} || X <- Deck]
					   )
				   ),
	Y.

	
%%
%% @doc
%% @private
%% Return full set card of the game
get_init_set() ->
	lists:flatten(
	[
	 % Обычные свойства
	 lists:duplicate(4, #card{prop=?prp_running}),
	 lists:duplicate(8, #card{prop=?prp_swimming}),
	 lists:duplicate(4, #card{prop=?prp_tail_loss}),
	 lists:duplicate(4, #card{prop=?prp_piracy}),
	 lists:duplicate(4, #card{prop=?prp_mimicry}),
	 lists:duplicate(4, #card{prop=?prp_scavanger}),	 
	 lists:duplicate(4, #card{prop=?prp_poisonous,prop_2=?prp_carnivouros}),
	 lists:duplicate(4, #card{prop=?prp_high_body_weight,prop_2=?prp_carnivouros}),
	 lists:duplicate(4, #card{prop=?prp_high_body_weight,prop_2=?prp_fat_tissue}),
	 lists:duplicate(4, #card{prop=?prp_burrowing,prop_2=?prp_fat_tissue}),
	 lists:duplicate(4, #card{prop=?prp_hibernation_ability,prop_2=?prp_carnivouros}),
	 lists:duplicate(4, #card{prop=?prp_sharp_vision,prop_2=?prp_fat_tissue}),
	 lists:duplicate(4, #card{prop=?prp_camouflage,prop_2=?prp_fat_tissue}),
	 lists:duplicate(4, #card{prop=?prp_grazing,prop_2=?prp_fat_tissue}),
     
	 % Болезни
	 lists:duplicate(4, #card{prop=?prp_parasite,prop_2=?prp_carnivouros}),
	 lists:duplicate(4, #card{prop=?prp_parasite,prop_2=?prp_fat_tissue}),
	 
	 % Групповые взаимодействия
	 lists:duplicate(4, #card{prop=?prp_symbiosus}),
	 lists:duplicate(4, #card{prop=?prp_cooperation,prop_2=?prp_carnivouros}),
	 lists:duplicate(4, #card{prop=?prp_cooperation,prop_2=?prp_fat_tissue}),	 
	 lists:duplicate(4, #card{prop=?prp_communication,prop_2=?prp_carnivouros})
	 ])
	.
	

%% ===================================
%% TESTS
%% ===================================	
	
init_set_test() ->
	?assertEqual(84, length(get_init_set())).

shuffle_deck_test() ->
	L=get_init_set(),
	SH1 = shuffle_deck(L),
	SH2 = shuffle_deck(L),
	?assertNotEqual(SH1,SH2).
	

