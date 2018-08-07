%% -----------------------------------------------------------------------------
%%
%% plsql_parser_dbss_test.erl: SQL - DBSS format test driver.
%%
%% Copyright (c) 2018-18 K2 Informatics GmbH.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
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
                {formatter("TEST_04", ?TEST_04, ?TEST_04_RESULT_DEFAULT, LOpts)}
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

real_world_test_() ->
    ?D("Start ~n"),
    LOpts = maps:from_list([{indent_space, 4}, {indent_with, space}]),
    {
        setup,
        fun setup_default/0,
        fun() ->
            [
                {formatter("TEST_12", ?TEST_12, ?TEST_12_RESULT_DEFAULT, LOpts)},
                {formatter("TEST_13", ?TEST_13, ?TEST_13_RESULT_DEFAULT, LOpts)},
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
