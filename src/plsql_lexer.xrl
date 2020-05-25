%% -----------------------------------------------------------------------------
%%
%% plsql_lexer.xrl: PL/SQL - lexer definition.
%%
%% Copyright (c) 2018-20 Konnexions GmbH.  All Rights Reserved.
%%
%% -----------------------------------------------------------------------------

%% -*- erlang -*-
Definitions.

Rules.

% $ELSE / $ELSIF / $END / $IF / $THEN
(\$[Ee][Ll][Ss][Ee])                                 : {token, {list_to_atom(string:uppercase(TokenChars)), TokenLine}}.
(\$[Ee][Ll][Ss][Ii][Ff])                             : {token, {list_to_atom(string:uppercase(TokenChars)), TokenLine}}.
(\$[Ee][Nn][Dd])                                     : {token, {list_to_atom(string:uppercase(TokenChars)), TokenLine}}.
(\$[Ii][Ff])                                         : {token, {list_to_atom(string:uppercase(TokenChars)), TokenLine}}.
(\$[Tt][Hh][Ee][Nn])                                 : {token, {list_to_atom(string:uppercase(TokenChars)), TokenLine}}.

% %ROWTYPE / %TYPE
(%[Rr][Oo][Ww][Tt][Yy][Pp][Ee])                      : {token, {list_to_atom(string:uppercase(TokenChars)), TokenLine}}.
(%[Tt][Yy][Pp][Ee])                                  : {token, {list_to_atom(string:uppercase(TokenChars)), TokenLine}}.

% strings
(\'([^\']*(\'\')*)*\')                               : {token, {'STRING', TokenLine, TokenChars}}.
(\"((\$|[^\"]*)*(\"\")*)*\")                         : {token, {'NAME', TokenLine, TokenChars}}.

% punctuation
(!=|\^=|<>|<|>|<=|>=)                                : {token, {'COMPARISON', TokenLine, list_to_atom(TokenChars)}}.
([=\|\-\+\*\/\(\)\,\.\;]|(\|\|)|(:=)|(=>)|(\-\-<>))  : {token, {list_to_atom(TokenChars), TokenLine}}.

% names
[A-Za-z][A-Za-z0-9_\$#]*                             : match_any(TokenChars, TokenLen, TokenLine, ?TOKENPATTERNS).

% parameters
(\:[A-Za-z0-9_\.][A-Za-z0-9_\.]*)                    : {token, {'PARAMETER', TokenLine, TokenChars}}.

% numbers
([0-9]+)                                             : {token, {'INTNUM', TokenLine, TokenChars}}.
((([\.][0-9]+)|([0-9]+[\.]?[0-9]*))([eE][+-]?[0-9]+)?[fFdD]?)
                                                     : {token, {'APPROXNUM', TokenLine, TokenChars}}.

% man_page
(/\*<>([^*]|[\r\n]|(\*+([^*/]|[\r\n])))*\*+/)        : {token, {'MAN_PAGE', TokenLine, TokenChars}}.

% skips
([\s\t\r\n]+)                                        : skip_token.    %% white space

% block comments
(/\*([^*]|[\r\n]|(\*+([^*/]|[\r\n])))*\*+/)          : skip_token.

% line comments
(--([\sa-zA-Z0-9\-].*[\r\n]+|[\r\n]+))               : skip_token.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Erlang code.

%% -----------------------------------------------------------------------------
%%
%% plsql_lexer.erl: PL/SQL - lexer.
%%
%% Copyright (c) 2018-20 Konnexions GmbH.  All Rights Reserved.
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
