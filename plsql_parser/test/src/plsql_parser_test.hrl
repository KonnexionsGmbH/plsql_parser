%% -----------------------------------------------------------------------------
%%
%% plsql_parser_test.hrl: PL/SQL - test data generator.
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

-ifndef(PLSQL_PARSER_TEST_HRL).
-define(PLSQL_PARSER_TEST_HRL, true).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("plsql_parser.hrl").

-define(ENV_VAR_FILE_TYPE, ".tst").
-define(ENV_VAR_FILE_WILDCARD, "SOURCEFILES").
-define(ENV_VAR_LOGGING_LEVEL, "LOG").
-define(PARSER_MODULE, plsql_parser).
-define(TIMEOUT, 60).

-endif.
