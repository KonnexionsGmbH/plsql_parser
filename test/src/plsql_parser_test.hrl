%% -----------------------------------------------------------------------------
%%
%% plsql_parser_test.hrl: PL/SQL - test data generator.
%%
%% Copyright (c) 2018-20 Konnexions GmbH.  All Rights Reserved.
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
