%% -----------------------------------------------------------------------------
%%
%% plsql_parse_format_dbss.hrl: SQL - creating a DBSS formatted version
%%                                    of the SQL statement.
%%
%% Copyright (c) 2018-20 Konnexions GmbH.  All Rights Reserved.
%%
%% -----------------------------------------------------------------------------

-ifndef(PLSQL_PARSER_FORMAT_PRETTY_HRL).
-define(PLSQL_PARSER_FORMAT_PRETTY_HRL, true).

-include("plsql_parser_fold.hrl").

-record(lopts, {
%% Allowed values: 1 - 8 -------------------------------------------------------
    indent_space = 4,
%% Allowed values: space, tab --------------------------------------------------
    indent_with = space
}).

-endif.
