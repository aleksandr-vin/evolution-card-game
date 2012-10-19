%%%-------------------------------------------------------------------
%%% @author Aleksandr Vinokurov <aleksandr.vin@gmail.com>
%%% @copyright (C) 2012, Aleksandr Vinokurov
%%% @doc
%%%
%%% @end
%%% Created : 20 Oct 2012 by Aleksandr Vinokurov <aleksandr.vin@gmail.com>
%%%-------------------------------------------------------------------
-module(evocg_creature_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    ok = evocg:start(),
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok = evocg:stop(),
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%% @end
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() -> 
    [sample_scenario_case].

%%--------------------------------------------------------------------
%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
sample_scenario_case() -> 
    [].

%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% @end
%%--------------------------------------------------------------------
sample_scenario_case(_Config) ->
    {Player1, Player2} = {"Jay", "Bob"},
    {ok, PlayerSup1} = evocg_sup:new_player(Player1),
    {ok, PlayerSup2} = evocg_sup:new_player(Player2),
    true = (PlayerSup1 =/= PlayerSup2),
    {ok, P1Critter1} = evocg_critters_sup:new_critter(Player1),
    {ok, P1Critter2} = evocg_critters_sup:new_critter(Player1),
    {ok, P2Critter1} = evocg_critters_sup:new_critter(Player2),
    {ok, P2Critter2} = evocg_critters_sup:new_critter(Player2),
    PidSet = sets:from_list([PlayerSup1, PlayerSup2,
			     P1Critter1, P1Critter2,
			     P2Critter1, P2Critter2]),
    6 = sets:size(PidSet), % All pids are unique
    ok.
