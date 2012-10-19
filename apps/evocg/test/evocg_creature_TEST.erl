-module(evocg_creature_TEST).

-include_lib("eunit/include/eunit.hrl").

naming_scenario_test() ->
    {ok, C1} = evocg_creature:start_link("Jay"),
    ?assertEqual(ok, evocg_creature:name(C1, "Goat")),
    ?assertEqual(ok, evocg_creature:name(C1, "Lion")).

carnivorous_scenario_test() ->
    {ok, C1} = evocg_creature:start_link("Jay"),
    ?assertEqual(ok, evocg_creature:name(C1, "Lion")),
    ?assertEqual(ok, evocg_creature:carnivorous(C1)).
