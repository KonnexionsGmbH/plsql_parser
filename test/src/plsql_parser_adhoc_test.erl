%% -----------------------------------------------------------------------------
%%
%% plsql_parser_adhoc_test.erl: PL/SQL - test driver for development purposes.
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

-module(plsql_parser_adhoc_test).

-export([eunit_test_source/1]).

-define(LOPTS, [
]).

-define(NODEBUG, true).

%% Possible values:
%%  - dbss,
%%  - flat,
%%  - full (= default value)
-define(TEST_VERSION, flat).

-include_lib("eunit/include/eunit.hrl").
-include("plsql_parser.hrl").
-include("plsql_parser_test.hrl").

%%------------------------------------------------------------------------------
%% EUnit Test Driver - Determine Files.
%%------------------------------------------------------------------------------

eunit_test_() ->
    ?D("Start~n"),

    {ok, Cwd} = file:get_cwd(),
    RootPath = lists:reverse(filename:split(Cwd)),
    TestDir = filename:join(lists:reverse(["test" | RootPath])),
    TestFile = filename:join(TestDir, ?MODULE_STRING ++ ?ENV_VAR_FILE_TYPE),

    {ok, [Opts | Tests]} = file:consult(TestFile),
    {ok, TestFileBin} = file:read_file(TestFile),
    TestLines = [begin
                     TRe0 = re:replace(T, "(.*)(\")(.*)", "\\1\\\\\"\\3",
                         [{return, list}, ungreedy, global, dotall]),
                     TRe = list_to_binary(io_lib:format("~p", [TRe0])),
                     case binary:match(TestFileBin, TRe) of
                         {I1, _} ->
                             <<Head:I1/binary, _/binary>> = TestFileBin,
                             case re:run(Head, ".*[\r\n]",
                                 [global]) of
                                 {match, Matches} -> length(Matches) + 1;
                                 nomatch ->
                                     io:format(user,
                                         "~p~n"
                                         ">>>>>>>>>>> HEAD ~p <<<<<<<<<<<~n"
                                         "Opts ~p~n"
                                         "Tests ~p~n"
                                         "T ~p~n"
                                         "TRe ~p~n"
                                         ,
                                         [TestFile, Head, Opts, Tests, T, TRe]),
                                     error(nomatch)
                             end;
                         nomatch -> I
                     end
                 end
        || {I, T} <- lists:zip(lists:seq(1, length(Tests)), Tests)],
    AugTests = lists:zip(TestLines, Tests),
    tests_gen(AugTests, Opts).

%%------------------------------------------------------------------------------
%% EUnit Test Driver - Processing Test Cases.
%%------------------------------------------------------------------------------

eunit_test_source(Source) ->
    ?D("Start~n Source: ~p~n", [Source]),
    io:format(user, "~n", []),
    io:format(user, ?MODULE_STRING ++
    " : ===========================================>           Test version:~n~ts~n~n",
        [?TEST_VERSION]),
    io:format(user, "~n", []),
    io:format(user, ?MODULE_STRING ++
    " : ===========================================>     Input statement(s):~n~ts~n~n",
        [Source]),

    try
        {ok, ParseTree} = plsql_parser:parsetree(Source),
        ?D("~n ParseTree: ~p~n", [ParseTree]),
        case ?TEST_VERSION of
            dbss ->
                binary_to_list(
                    plsql_parser_fold:top_down(plsql_parser_format_dbss,
                        ParseTree, []));
            full ->
                plsql_parser_test_utils:eunit_test(Source);
            _ ->
                binary_to_list(
                    plsql_parser_fold:top_down(plsql_parser_format_flat,
                        ParseTree, []))
        end
    of
        {error, Reason} ->
            io:format(user, "~n", []),
            io:format(user, ?MODULE_STRING ++
            " : -------------------------------------------> Statement error reason:~n~p~n~n",
                [Reason]),
            erlang:error(Reason);
        {ok, Result} ->
            io:format(user, "~n", []),
            io:format(user, ?MODULE_STRING ++
            " : ------------------------------------------->    Output statement(s):~n~ts~n~n",
                [Result]),
            case ?TEST_VERSION of
                dbss -> ok;
                _ -> {ok, ParseTree1} = plsql_parser:parsetree(Source),
                    {ok, ParseTree2} = plsql_parser:parsetree(Result),
                    case ParseTree1 == ParseTree2 of
                        true -> true;
                        _ ->
                            ?E(
                                "ParseTree1 /= ParseTree2 ===>~n ParseTree1: ~p~n ParseTree2: ~p~n",
                                [ParseTree1, ParseTree2]),
                            ParseTree1 = ParseTree2
                    end
            end;
        Result ->
            io:format(user, "~n", []),
            io:format(user, ?MODULE_STRING ++
            " : ------------------------------------------->    Output statement(s):~n~ts~n~n",
                [Result]),
            case ?TEST_VERSION of
                dbss -> ok;
                _ ->
                    {ok, ParseTree1} = plsql_parser:parsetree(Source),
                    {ok, ParseTree2} = plsql_parser:parsetree(Result),
                    case ParseTree1 == ParseTree2 of
                        true -> true;
                        _ ->
                            ?E(
                                "ParseTree1 /= ParseTree2 ===>~n ParseTree1: ~p~n ParseTree2: ~p~n",
                                [ParseTree1, ParseTree2]),
                            ParseTree1 = ParseTree2
                    end
            end
    catch
        error:Reason:Stacktrace ->
            io:format(user, "~n", []),
            io:format(user, ?MODULE_STRING ++
            " : -------------------------------------------> Statement catch reason:~n~p~n~n",
                [Reason]),
            io:format(user, ?MODULE_STRING ++
            " : -------------------------------------------> Current stacktrace:~n~p~n~n",
                [Stacktrace]),
            erlang:error(Reason)
    end.

%%------------------------------------------------------------------------------
%% EUnit Test Driver - Processing Groups.
%%------------------------------------------------------------------------------

tests_gen(Tests, Opts) ->
    ?D("Start~n Tests: ~p~n Opts: ~p~n", [Tests, Opts]),
    SelTests = case proplists:get_value(tests, Opts) of
                   St when St =:= undefined; St =:= [] ->
                       {Indices, _} = lists:unzip(Tests),
                       Indices;
                   St -> St
               end,
    tests_gen(Tests, SelTests, []).

tests_gen([], _SelTests, Acc) ->
    {inorder, lists:reverse(Acc)};
tests_gen([{I, T} | Tests], SelTests, Acc) ->
    case lists:member(I, SelTests) of
        true ->
            tests_gen(Tests, SelTests, [{I, fun() ->
                {timeout, ?TIMEOUT, eunit_test_source(T)} end} | Acc]);
        _ -> Acc
    end.
