%% -----------------------------------------------------------------------------
%%
%% plsql_parser_dbss_test.erl: SQL - DBSS format test driver.
%%
%% Copyright (c) 2018-20 Konnexions GmbH.  All Rights Reserved.
%%
%% -----------------------------------------------------------------------------

-module(plsql_parser_dbss_test).

-define(NODEBUG, true).

-include("plsql_parser_dbss_test.hrl").

simple_test_() ->
    ?D("Start ~n"),
    LOpts = maps:from_list([{indent_space, 4}, {indent_with, space}]),
    {
        setup,
        fun setup_default/0,
        fun() ->
            [
                {formatter("TEST_01", ?TEST_01, ?TEST_01_RESULT_DEFAULT, [])},
                {formatter("TEST_02", ?TEST_02, ?TEST_02_RESULT_DEFAULT, [])},
                {formatter("TEST_03", ?TEST_03, ?TEST_03_RESULT_DEFAULT, [])},
                {formatter("TEST_04", ?TEST_04, ?TEST_04_RESULT_DEFAULT, LOpts)},
                {formatter("TEST_05", ?TEST_05, ?TEST_05_RESULT_DEFAULT, [])},
                {formatter("TEST_06", ?TEST_06, ?TEST_06_RESULT_DEFAULT, [])},
                {formatter("TEST_07", ?TEST_07, ?TEST_07_RESULT_DEFAULT, [])}
            ]
        end
    }.

complete_test_() ->
    ?D("Start ~n"),
    LOpts = [{indent_space, 4}, {indent_with, space}],
    {
        setup,
        fun setup_default/0,
        fun() ->
            [
                {formatter("TEST_11", ?TEST_11, ?TEST_11_RESULT_DEFAULT, LOpts)}
            ]
        end
    }.

real_world_12_test_() ->
    ?D("Start ~n"),
    LOpts = maps:from_list([{indent_space, 4}, {indent_with, space}]),
    {
        setup,
        fun setup_default/0,
        fun() ->
            [
                {formatter("TEST_12", ?TEST_12, ?TEST_12_RESULT_DEFAULT, LOpts)}
            ]
        end
    }.

real_world_13_test_() ->
    ?D("Start ~n"),
    LOpts = maps:from_list([{indent_space, 4}, {indent_with, space}]),
    {
        setup,
        fun setup_default/0,
        fun() ->
            [
                {formatter("TEST_13", ?TEST_13, ?TEST_13_RESULT_DEFAULT, LOpts)}
            ]
        end
    }.

real_world_14_test_() ->
    ?D("Start ~n"),
    LOpts = maps:from_list([{indent_space, 4}, {indent_with, space}]),
    {
        setup,
        fun setup_default/0,
        fun() ->
            [
                {formatter("TEST_14", ?TEST_14, ?TEST_14_RESULT_DEFAULT, LOpts)}
            ]
        end
    }.

%%------------------------------------------------------------------------------
%% Helper functions.
%%------------------------------------------------------------------------------

formatter(Title, Source, Result, LOpts) ->
    ?D("Start ~n Title: ~p~n Source: ~p~n Result: ~p~n LOpts: ~p~n",
        [Title, Source, Result, LOpts]),
    case plsql_parser_test_utils:eunit_test(Source, LOpts, dbss) of
        {ok, Source_Format} ->
            ?assertEqual(Result, binary_to_list(Source_Format));
        ErrorResult ->
            io:format(user, "~n" ++ ?MODULE_STRING ++
                " : Error in eunit_test : Title      ~n > ~p~n", [Title]),
            io:format(user, "~n" ++ ?MODULE_STRING ++
                " : Error in eunit_test : ErrorResult~n > ~p~n", [ErrorResult])
    end.

%%------------------------------------------------------------------------------
%% Setup functions.
%%------------------------------------------------------------------------------

setup_default() ->
    ?D("Start ~n"),
    ok.
