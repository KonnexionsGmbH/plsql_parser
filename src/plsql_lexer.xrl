%% -----------------------------------------------------------------------------
%%
%% plsql_lexer.xrl: PL/SQL - lexer definition.
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

%% -*- erlang -*-
Definitions.

Rules.

% $ELSE / $ELSIF / $END / $IF / $THEN
(\$[Ee][Ll][Ss][Ee])                                : {token, {list_to_atom(string:uppercase(TokenChars)), TokenLine}}.
(\$[Ee][Ll][Ss][Ii][Ff])                            : {token, {list_to_atom(string:uppercase(TokenChars)), TokenLine}}.
(\$[Ee][Nn][Dd])                                    : {token, {list_to_atom(string:uppercase(TokenChars)), TokenLine}}.
(\$[Ii][Ff])                                        : {token, {list_to_atom(string:uppercase(TokenChars)), TokenLine}}.
(\$[Tt][Hh][Ee][Nn])                                : {token, {list_to_atom(string:uppercase(TokenChars)), TokenLine}}.

% %ROWTYPE / %TYPE
(%[Rr][Oo][Ww][Tt][Yy][Pp][Ee])                     : {token, {list_to_atom(string:uppercase(TokenChars)), TokenLine}}.
(%[Tt][Yy][Pp][Ee])                                 : {token, {list_to_atom(string:uppercase(TokenChars)), TokenLine}}.

% strings
(\'([^\']*(\'\')*)*\')                              : {token, {'STRING', TokenLine, TokenChars}}.
(\"((\$|[^\"]*)*(\"\")*)*\")                        : {token, {'NAME', TokenLine, TokenChars}}.

% punctuation
(!=|\^=|<>|<|>|<=|>=)                               : {token, {'COMPARISON', TokenLine, list_to_atom(TokenChars)}}.
([=\|\-\+\*\/\(\)\,\.\;]|(\|\|)|(:=)|(=>)|(\-\-<>)) : {token, {list_to_atom(TokenChars), TokenLine}}.

% names
[A-Za-z][A-Za-z0-9_\$@~]*                           : match_any(TokenChars, TokenLen, TokenLine, ?TOKENPATTERNS).

% parameters
(\:[A-Za-z0-9_\.][A-Za-z0-9_\.]*)                   : {token, {'PARAMETER', TokenLine, TokenChars}}.

% numbers
([0-9]+)                                            : {token, {'INTNUM', TokenLine, TokenChars}}.
((([\.][0-9]+)|([0-9]+[\.]?[0-9]*))([eE][+-]?[0-9]+)?[fFdD]?)
                                                    : {token, {'APPROXNUM', TokenLine, TokenChars}}.

% skips
([\s\t\r\n]+)                                       : skip_token.    %% white space

% block comments
(/\*([^*]|[\r\n]|(\*+([^*/]|[\r\n])))*\*+/)         : skip_token.

% line comments
(--(\s.*[\r\n]+|[\r\n]+))                                          : skip_token.
%(--\n)                                          : skip_token.
%(--\r\n)                                          : skip_token.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Erlang code.

%% -----------------------------------------------------------------------------
%%
%% plsql_lexer.erl: PL/SQL - lexer.
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

-export([reserved_keywords/0]).

-define(NODEBUG, true).
-include("plsql_lexer.hrl").

reserved_keywords() -> [T || {_, T} <- ?TOKENPATTERNS].

match_any(TokenChars, TokenLen, _TokenLine, []) ->
    {token, {'NAME', TokenLen, TokenChars}};
match_any(TokenChars, TokenLen, TokenLine, [{P, T} | TPs]) ->
    case re:run(TokenChars, P, [{capture, first, list}]) of
        {match, [_]} -> {token, {T, TokenLine}};
        nomatch -> match_any(TokenChars, TokenLen, TokenLine, TPs)
    end.
