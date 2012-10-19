-module(evocg).

%% API
-export([start/0, stop/0]).

%% ===================================================================
%% API
%% ===================================================================
start() ->
    ok = lists:foreach(fun (A) -> ok = application:start(A) end,
                       applications()).

stop() ->
    ok = lists:foreach(fun (A) -> ok = application:stop(A) end,
                       lists:reverse(applications())).

%% ===================================================================
%% Internals
%% ===================================================================
applications() ->
    [syntax_tools, compiler, lager, evocg].
