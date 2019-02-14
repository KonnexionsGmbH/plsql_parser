%% -----------------------------------------------------------------------------
%%
%% plsql_lexer.hrl: PL/SQL - lexer definition.
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

-ifndef(PLSQL_LEXER_HRL).
-define(PLSQL_LEXER_HRL, true).

-include("plsql_parser.hrl").

-define(TOKENPATTERNS, [
    {"^(?i)(ACCESSIBLE)$",                  'ACCESSIBLE'},
    {"^(?i)(AND)$",                         'AND'},
    {"^(?i)(ANY)$",                         'ANY'},
    {"^(?i)(API_GROUP)$",                   'API_GROUP'},
    {"^(?i)(API_HIDDEN)$",                  'API_HIDDEN'},
    {"^(?i)(AS)$",                          'AS'},
    {"^(?i)(AUTHID)$",                      'AUTHID'},
    {"^(?i)(BFILE)$",                       'BFILE'},
    {"^(?i)(BINARY_DOUBLE)$",               'BINARY_DOUBLE'},
    {"^(?i)(BINARY_FLOAT)$",                'BINARY_FLOAT'},
    {"^(?i)(BINARY_INTEGER)$",              'BINARY_INTEGER'},
    {"^(?i)(BITMAP)$",                      'BITMAP'},
    {"^(?i)(BLOB)$",                        'BLOB'},
    {"^(?i)(BOOLEAN)$",                     'BOOLEAN'},
    {"^(?i)(BY)$",                          'BY'},
    {"^(?i)(BYTE)$",                        'BYTE'},
    {"^(?i)(CHAR)$",                        'CHAR'},
    {"^(?i)(CLOB)$",                        'CLOB'},
    {"^(?i)(CLUSTER)$",                     'CLUSTER'},
    {"^(?i)(COLLATION)$",                   'COLLATION'},
    {"^(?i)(CONSTANT)$",                    'CONSTANT'},
    {"^(?i)(CREATE)$",                      'CREATE'},
    {"^(?i)(CURRENT_USER)$",                'CURRENT_USER'},
    {"^(?i)(CURSOR)$",                      'CURSOR'},
    {"^(?i)(DATE)$",                        'DATE'},
    {"^(?i)(DAY)$",                         'DAY'},
    {"^(?i)(DEFAULT)$",                     'DEFAULT'},
    {"^(?i)(DEFINER)$",                     'DEFINER'},
    {"^(?i)(DETERMINISTIC)$",               'DETERMINISTIC'},
    {"^(?i)(EDITIONABLE)$",                 'EDITIONABLE'},
    {"^(?i)(END)$",                         'END'},
    {"^(?i)(EXCEPTION)$",                   'EXCEPTION'},
    {"^(?i)(FALSE)$",                       'FALSE'},
    {"^(?i)(FLOAT)$",                       'FLOAT'},
    {"^(?i)(FUNCTION)$",                    'FUNCTION'},
    {"^(?i)(HASH)$",                        'HASH'},
    {"^(?i)(IN)$",                          'IN'},
    {"^(?i)(INDICATOR)$",                   'INDICATOR'},
    {"^(?i)(INTERVAL)$",                    'INTERVAL'},
    {"^(?i)(IS)$",                          'IS'},
    {"^(?i)(LOCAL)$",                       'LOCAL'},
    {"^(?i)(LONG)$",                        'LONG'},
    {"^(?i)(METADATA)$",                    'METADATA'},
    {"^(?i)(MONTH)$",                       'MONTH'},
    {"^(?i)(NAME)$",                        'NAME'},
    {"^(?i)(NCHAR)$",                       'NCHAR'},
    {"^(?i)(NCLOB)$",                       'NCLOB'},
    {"^(?i)(NOCOPY)$",                      'NOCOPY'},
    {"^(?i)(NONE)$",                        'NONE'},
    {"^(?i)(NONEDITIONABLE)$",              'NONEDITIONABLE'},
    {"^(?i)(NOT)$",                         'NOT'},
    {"^(?i)(NULL)$",                        'NULLX'},
    {"^(?i)(NUMBER)$",                      'NUMBER'},
    {"^(?i)(NVARCHAR2)$",                   'NVARCHAR2'},
    {"^(?i)(OBJECT_PRIVILEGE)$",            'OBJECT_PRIVILEGE'},
    {"^(?i)(OR)$",                          'OR'},
    {"^(?i)(ORDER)$",                       'ORDER'},
    {"^(?i)(OUT)$",                         'OUT'},
    {"^(?i)(PACKAGE)$",                     'PACKAGE'},
    {"^(?i)(PARALLEL_ENABLED)$",            'PARALLEL_ENABLED'},
    {"^(?i)(PARTITION)$",                   'PARTITION'},
    {"^(?i)(PIPELINED)$",                   'PIPELINED'},
    {"^(?i)(PLS_INTEGER)$",                 'PLS_INTEGER'},
    {"^(?i)(POLYMORPHIC)$",                 'POLYMORPHIC'},
    {"^(?i)(PROCEDURE)$",                   'PROCEDURE'},
    {"^(?i)(RANGE)$",                       'RANGE'},
    {"^(?i)(RAW)$",                         'RAW'},
    {"^(?i)(REF)$",                         'REF'},
    {"^(?i)(RELIES_ON)$",                   'RELIES_ON'},
    {"^(?i)(REPLACE)$",                     'REPLACE'},
    {"^(?i)(RESULT_CACHE)$",                'RESULT_CACHE'},
    {"^(?i)(RETURN)$",                      'RETURN'},
    {"^(?i)(ROW)$",                         'ROW'},
    {"^(?i)(ROWID)$",                       'ROWID'},
    {"^(?i)(SECOND)$",                      'SECOND'},
    {"^(?i)(SHARING)$",                     'SHARING'},
    {"^(?i)(SYSTEM_PRIVILEGE)$",            'SYSTEM_PRIVILEGE'},
    {"^(?i)(TABLE)$",                       'TABLE'},
    {"^(?i)(TIME)$",                        'TIME'},
    {"^(?i)(TIMESTAMP)$",                   'TIMESTAMP'},
    {"^(?i)(TO)$",                          'TO'},
    {"^(?i)(TRIGGER)$",                     'TRIGGER'},
    {"^(?i)(TRUE)$",                        'TRUE'},
    {"^(?i)(TYPE)$",                        'TYPE'},
    {"^(?i)(UROWID)$",                      'UROWID'},
    {"^(?i)(USING)$",                       'USING'},
    {"^(?i)(USING_NLS_COMP)$",              'USING_NLS_COMP'},
    {"^(?i)(VALUE)$",                       'VALUE'},
    {"^(?i)(VARCHAR2)$",                    'VARCHAR2'},
    {"^(?i)(WITH)$",                        'WITH'},
    {"^(?i)(XMLTYPE)$",                     'XMLTYPE'},
    {"^(?i)(YEAR)$",                        'YEAR'},
    {"^(?i)(ZONE)$",                        'ZONE'}
]).

-endif.
