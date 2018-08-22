%% -----------------------------------------------------------------------------
%%
%% plsql_parser_test_utils.erl: PL/SQL - test driver utilities.
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

-module(plsql_parser_test_utils).

-export([
    eunit_test/1,
    eunit_test/2,
    eunit_test/3
]).

-define(NODEBUG, true).

-include_lib("eunit/include/eunit.hrl").
-include("plsql_parser.hrl").
-include("plsql_parser_test.hrl").

%%------------------------------------------------------------------------------
%% EUnit Test Driver - Processing Test Cases.
%%------------------------------------------------------------------------------

eunit_test(Source) ->
    eunit_test(Source, []).

eunit_test(Source, LOpts) ->
    eunit_test(Source, LOpts, flat).

eunit_test(Source, LOpts, Type) ->
    ?D("Start ~nSource: ~p~nLOpts: ~p~nType: ~p~n", [Source, LOpts, Type]),
    %% -------------------------------------------------------------------------
    %% 1. Source ==> ParseTree
    %% -------------------------------------------------------------------------
    case ?PARSER_MODULE:parsetree_with_tokens(Source) of
        {ok, {ParseTree, Tokens}} ->
            ?E("~n ParseTree: ~p~n Tokens: ~p~n", [ParseTree, Tokens]),
            ?D("~n ParseTree: ~p~n Tokens: ~p~n", [ParseTree, Tokens]),
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
                        io:format(user, "~n" ++ ?MODULE_STRING ++
                            " : [TD] Error ParseTree ==> Source_TD : Error    ~n > ~p",
                            [Error_1_TD]),
                        io:format(user, "~n" ++ ?MODULE_STRING ++
                            " : [TD] Error ParseTree ==> Source_TD : Source   ~n > ~p",
                            [Source]),
                        io:format(user, "~n" ++ ?MODULE_STRING ++
                            " : [TD] Error ParseTree ==> Source_TD : ParseTree~n > ~p",
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
                          io:format(user, "~n" ++ ?MODULE_STRING ++
                              " : [TD] Error Source_TD ==> ParseTree_TD : Exception~n > ~p",
                              [Exception_TD]),
                          io:format(user, "~n" ++ ?MODULE_STRING ++
                              " : [TD] Error Source_TD ==> ParseTree_TD : Reason   ~n > ~p",
                              [Reason_TD]),
                          io:format(user, "~n" ++ ?MODULE_STRING ++
                              " : [TD] Error Source_TD ==> ParseTree_TD : Source   ~n > ~p",
                              [Source]),
                          io:format(user, "~n" ++ ?MODULE_STRING ++
                              " : [TD] Error Source_TD ==> ParseTree_TD : Source_TD~n > ~p",
                              [Source_TD])
                  end,
            %% -----------------------------------------------------------------
            %% 4. ParseTree == ParseTree_TD ?
            %% -----------------------------------------------------------------
            if ParseTree /= ParseTree_TD ->
                io:format(user, "~n" ++ ?MODULE_STRING ++
                    " : [TD] Error ParseTree /= ParseTree_TD : Source      ~n > ~p",
                    [Source]),
                io:format(user, "~n" ++ ?MODULE_STRING ++
                    " : [TD] Error ParseTree /= ParseTree_TD : Source_TD   ~n > ~p",
                    [Source_TD]),
                io:format(user, "~n" ++ ?MODULE_STRING ++
                    " : [TD] Error ParseTree /= ParseTree_TD : ParseTree   ~n > ~p",
                    [ParseTree]),
                io:format(user, "~n" ++ ?MODULE_STRING ++
                    " : [TD] Error ParseTree /= ParseTree_TD : ParseTree_TD~n > ~p",
                    [ParseTree_TD]),
                io:format(user, "~n" ++ ?MODULE_STRING ++
                    " : [TD] Error ParseTree /= ParseTree_TD : Tokens      ~n > ~p",
                    [Tokens]),
                io:format(user, "~n" ++ ?MODULE_STRING ++
                    " : [TD] Error ParseTree /= ParseTree_TD : Tokens_TD   ~n > ~p",
                    [Tokens_TD]),
                throw("[TD] Error ParseTree /= ParseTree_TD");
                true -> ok
            end,
            ?assertEqual(ParseTree, ParseTree_TD),
            %% -----------------------------------------------------------------
            %% 5. No redundant whitespaces.
            %% -----------------------------------------------------------------
            Source_TD_MultipleSpace = string:str(Source_TD, "  "),
            case Source_TD_MultipleSpace of
                0 -> ok;
                _ ->
                    io:format(user, "~n" ++ ?MODULE_STRING ++
                        " : [TD] Error redundant whitespace(s) : 1. Redundant WS~n > ~p",
                        [Source_TD_MultipleSpace]),
                    io:format(user, "~n" ++ ?MODULE_STRING ++
                        " : [TD] Error redundant whitespace(s) : Source         ~n > ~p",
                        [Source]),
                    io:format(user, "~n" ++ ?MODULE_STRING ++
                        " : [TD] Error redundant whitespace(s) : Source_TD      ~n > ~p",
                        [Source_TD]),
                    throw("[TD] Error redundant whitespace(s)")
            end,
            %% -----------------------------------------------------------------
            %% Test DBSS
            %% -----------------------------------------------------------------
            %% 6. ParseTree ==> Source_FORMAT
            %% -----------------------------------------------------------------
            Source_FORMAT =
                case plsql_parser_fold:top_down(plsql_parser_format_dbss,
                    ParseTree, LOpts) of
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
                end,
            {ok, case Type of
                     dbss -> Source_FORMAT;
                     _ -> Source_TD
                 end};
        {lex_error, _Error} ->
            io:format(user,
                "~n" ++ ?MODULE_STRING ++ " : Failed lex_error : Source~n > ~p",
                [Source]),
            io:format(user, "~n" ++
                ?MODULE_STRING ++ " : Failed lex_error : Error    ~n > ~p",
                [_Error]),
            throw({error, "Failed lex_error"});
        {parse_error, {_Error, Tokens}} ->
            io:format(user, "~n" ++
                ?MODULE_STRING ++ " : Failed parse_error : Source~n > ~p",
                [Source]),
            io:format(user, "~n" ++
                ?MODULE_STRING ++ " : Failed parse_error : Tokens   ~n > ~p",
                [Tokens]),
            io:format(user, "~n" ++
                ?MODULE_STRING ++ " : Failed parse_error : Error    ~n > ~p",
                [_Error]),
            throw({error, "Failed parse_error"})
    end.
