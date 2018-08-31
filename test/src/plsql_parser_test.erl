%% -----------------------------------------------------------------------------
%%
%% plsql_parser_test.erl: PL/SQL - test driver.
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

-module(plsql_parser_test).

-export([common_test_source/1]).
-export([eunit_test_source/2]).

-define(NODEBUG, true).

-include("plsql_parser_test.hrl").

%%------------------------------------------------------------------------------
%% Common Test Driver.
%%------------------------------------------------------------------------------

common_test_source(Source) ->
    % ct:pal(info, ?MAX_IMPORTANCE,
    %     ?MODULE_STRING ++ ":common_test_source ===>~n Source = ~p~n", [Source]),
    %% -------------------------------------------------------------------------
    %% 1. Source ==> ParseTree
    %% -------------------------------------------------------------------------
    case ?PARSER_MODULE:parsetree_with_tokens(Source) of
        {ok, {ParseTree, Tokens}} ->
            %% -----------------------------------------------------------------
            %% Test TopDown
            %% -----------------------------------------------------------------
            %% 2. ParseTree ==> Source_TD
            %% -----------------------------------------------------------------
            Source_TD =
                case plsql_parser_fold:top_down(plsql_parser_format_flat,
                    ParseTree,
                    []) of
                    {error, Error_1_TD} ->
                        ct:pal(
                            "[TD] Error ParseTree ==> Source_TD : Error    ~n > ~p~n",
                            [Error_1_TD]),
                        ct:pal(
                            "[TD] Error ParseTree ==> Source_TD : Source   ~n > ~p~n",
                            [Source]),
                        ct:pal(
                            "[TD] Error ParseTree ==> Source_TD : ParseTree~n > ~p~n",
                            [ParseTree]),
                        throw("[TD] Error ParseTree ==> Source_TD");
                    NS_TD ->
                        binary_to_list(NS_TD)
                end,
            %% -----------------------------------------------------------------
            %% 3. Source_TD ==> ParseTree_TD
            %% -----------------------------------------------------------------
            {ok, {ParseTree_TD, Tokens_TD}}
                = try
                      case ?PARSER_MODULE:parsetree_with_tokens(Source_TD) of
                          {ok, RT_TD} -> {ok, RT_TD};
                          Error_2_TD -> throw(Error_2_TD)
                      end
                  catch
                      Exception_TD:Reason_TD ->
                          ct:pal(
                              "[TD] Error Source_TD ==> ParseTree_TD : Exception~n > ~p~n",
                              [Exception_TD]),
                          ct:pal(
                              "[TD] Error Source_TD ==> ParseTree_TD : Reason   ~n > ~p~n",
                              [Reason_TD]),
                          ct:pal(
                              "[TD] Error Source_TD ==> ParseTree_TD : Source   ~n > ~p~n",
                              [Source]),
                          ct:pal(
                              "[TD] Error Source_TD ==> ParseTree_TD : Source_TD~n > ~p~n",
                              [Source_TD])
                  end,
            %% -----------------------------------------------------------------
            %% 4. ParseTree == ParseTree_TD ?
            %% -----------------------------------------------------------------
            if ParseTree /= ParseTree_TD ->
                ct:pal(
                    "[TD] Error ParseTree /= ParseTree_TD : Source      ~n > ~p~n",
                    [Source]),
                ct:pal(
                    "[TD] Error ParseTree /= ParseTree_TD : Source_TD   ~n > ~p~n",
                    [Source_TD]),
                ct:pal(
                    "[TD] Error ParseTree /= ParseTree_TD : ParseTree   ~n > ~p~n",
                    [ParseTree]),
                ct:pal(
                    "[TD] Error ParseTree /= ParseTree_TD : ParseTree_TD~n > ~p~n",
                    [ParseTree_TD]),
                ct:pal(
                    "[TD] Error ParseTree /= ParseTree_TD : Tokens      ~n > ~p~n",
                    [Tokens]),
                ct:pal(
                    "[TD] Error ParseTree /= ParseTree_TD : Tokens_TD   ~n > ~p~n",
                    [Tokens_TD]),
                throw("[TD] Error ParseTree /= ParseTree_TD");
                true -> ok
            end,
            ?assertEqual(ParseTree, ParseTree_TD),
            %% -----------------------------------------------------------------
            %% Test DBSS
            %% -----------------------------------------------------------------
            %% 5. ParseTree ==> Source_FORMAT
            %% -----------------------------------------------------------------
            _Source_FORMAT =
                case plsql_parser_fold:top_down(plsql_parser_format_dbss,
                    ParseTree, []) of
                    {error, Error_1_FORMAT} ->
                        io:format(user, "~n" ++ ?MODULE_STRING ++
                            " : [FORMAT] Error ParseTree ==> Source_FORMAT : Error    ~n > ~p",
                            [Error_1_FORMAT]),
                        io:format(user, "~n" ++ ?MODULE_STRING ++
                            " : [FORMAT] Error ParseTree ==> Source_FORMAT : Source   ~n > ~p",
                            [Source]),
                        io:format(user, "~n" ++ ?MODULE_STRING ++
                            " : [FORMAT] Error ParseTree ==> Source_FORMAT : ParseTree~n > ~p",
                            [ParseTree]),
                        throw({error, Error_1_FORMAT});
                    NS_FORMAT -> NS_FORMAT
                end;
        {lex_error, Error} ->
            ct:pal("Failed lex_error : Source~n > ~p~n", [Source]),
            ct:pal("Failed lex_error : Error ~n > ~p~n", [Error]),
            throw({error, "Failed lex_error"});
        {parse_error, {Error, Tokens}} ->
            ct:pal("Failed parse_error : Source~n > ~p~n", [Source]),
            ct:pal("Failed parse_error : Tokens~n > ~p~n", [Tokens]),
            ct:pal("Failed parse_error : Error ~n > ~p~n", [Error]),
            throw({error, "Failed parse_error"})
    end.

%%------------------------------------------------------------------------------
%% EUnit Test Driver - Determine Files.
%%------------------------------------------------------------------------------

eunit_test_() ->
    WCard = case os:getenv(?ENV_VAR_FILE_WILDCARD) of
                SourceFiles when is_list(SourceFiles) ->
                    SourceFiles;
                _ ->
                    "*"
            end ++ ?ENV_VAR_FILE_TYPE,
    {ok, Cwd} = file:get_cwd(),
    RootPath = lists:reverse(filename:split(Cwd)),
    TestDir1 = filename:join(lists:reverse(["test" | RootPath])),
    TestDir2 = filename:join(
        lists:reverse(["eunit", "generated", "test"] ++ RootPath)),
    TestFiles = lists:sort(
        [filename:join(TestDir1, T) || T <- filelib:wildcard(WCard,
            TestDir1)] ++
        [filename:join(TestDir2, T) || T <- filelib:wildcard(WCard, TestDir2)]
    ),
    group_gen(TestFiles).

%%------------------------------------------------------------------------------
%% EUnit Test Driver - Processing Test Cases.
%%------------------------------------------------------------------------------

eunit_test_source(TestGroup, Source) ->
    ?D("Start~n TestGroup: ~p~n Source: ~p~n", [TestGroup, Source]),
    case plsql_parser_test_utils:eunit_test(Source) of
        {ok, _} -> ok;
        Result ->
            io:format(user, "~n" ++ ?MODULE_STRING ++
                " : Error in eunit_test_source : ErrorResult~n > ~p~n",
                [Result]),
            io:format(user, "~n" ++ ?MODULE_STRING ++
                " : Error in eunit_test_source : TestGroup  ~n > ~p~n",
                [TestGroup])
    end.

%%------------------------------------------------------------------------------
%% EUnit Test Driver - Processing Files.
%%------------------------------------------------------------------------------

group_gen(TestFiles) ->
    {generator,
        fun() ->
            case TestFiles of
                [] ->
                    [];
                [TestFile | RestTestFiles] ->
                    {ok, [Opts | Tests]} = file:consult(TestFile),
                    {ok, TestFileBin} = file:read_file(TestFile),
                    TestLines = [begin
                                     TRe0 = re:replace(T, "(.*)(\")(.*)",
                                         "\\1\\\\\"\\3"
                                         ,
                                         [
                                             {return, list},
                                             ungreedy,
                                             global,
                                             dotall
                                         ]),
                                     TRe = list_to_binary(
                                         io_lib:format("~p", [TRe0])),
                                     case binary:match(TestFileBin, TRe) of
                                         {I1, _} ->
                                             <<Head:I1/binary, _/binary>> =
                                                 TestFileBin,
                                             case re:run(Head, ".*[\r\n]",
                                                 [global]) of
                                                 {match, Matches} ->
                                                     length(Matches) + 1;
                                                 nomatch ->
                                                     io:format(user,
                                                         "~p~n"
                                                         ">>>>>>>>>>> HEAD ~p <<<<<<<<<<<~n"
                                                         "Opts ~p~n"
                                                         "Tests ~p~n"
                                                         "T ~p~n"
                                                         "TRe ~p~n",
                                                         [
                                                             TestFile,
                                                             Head,
                                                             Opts,
                                                             Tests,
                                                             T,
                                                             TRe
                                                         ]),
                                                     error(nomatch)
                                             end;
                                         nomatch ->
                                             I
                                     end
                                 end
                        || {I, T} <- lists:zip(lists:seq(1, length(Tests)),
                            Tests)],
                    AugTests = lists:zip(TestLines, Tests),
                    TestGroup = filename:rootname(
                        filename:basename(TestFile)),
                    [tests_gen(TestGroup, AugTests, Opts) | group_gen(
                        RestTestFiles)]
            end
        end}.

%%------------------------------------------------------------------------------
%% EUnit Test Driver - Processing Groups.
%%------------------------------------------------------------------------------

tests_gen(TestGroup, Tests, Opts) ->
    SelTests = case proplists:get_value(tests, Opts) of
                   St when St =:= undefined; St =:= [] ->
                       {Indices, _} = lists:unzip(Tests),
                       Indices;
                   St -> St
               end,
    tests_gen(TestGroup, Tests, SelTests, []).

tests_gen(_TestGroup, [], _SelTests, Acc) ->
    {inorder, lists:reverse(Acc)};
tests_gen(TestGroup, [{I, T} | Tests], SelTests, Acc) ->
    case lists:member(I, SelTests) of
        true ->
            tests_gen(TestGroup, Tests, SelTests,
                [{TestGroup, I,
                    fun() ->
                        {timeout, ?TIMEOUT, ?MODULE:eunit_test_source(TestGroup,
                            T)}
                    end} | Acc]);
        _ -> Acc
    end.
