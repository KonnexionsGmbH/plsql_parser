%% -----------------------------------------------------------------------------
%%
%% plsql_parse_format_dbss.hrl: SQL - creating a DBSS formatted version
%%                                    of the SQL statement.
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
