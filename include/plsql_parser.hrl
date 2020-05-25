%% -----------------------------------------------------------------------------
%%
%% plsql_parser.hrl: PL/SQL - unparsing utilities.
%%
%% Copyright (c) 2018-20 Konnexions GmbH.  All Rights Reserved.
%%
%% -----------------------------------------------------------------------------

-ifndef(PLSQL_PARSER_HRL).
-define(PLSQL_PARSER_HRL, true).

-ifdef(NODEBUG).
    -define(D(Format), undefined).
    -define(D(Format, Args), undefined).
-else.
    -define(D(Format), ?D(Format, [])).
    -define(D(Format, Args),
        io:format(user, "~p:~p:~p ===> "Format,
                  [?MODULE, ?FUNCTION_NAME, ?LINE | Args])).
-endif.

-define(E(Format), ?D(Format, [])).
-define(E(Format, Args),
    io:format(user, "~p:~p:~p ===> "Format,
               [?MODULE, ?FUNCTION_NAME, ?LINE | Args])).

-endif.
