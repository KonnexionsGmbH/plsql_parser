%% -----------------------------------------------------------------------------
%%
%% plsql_parser_fold.hrl: PL/SQL - unparsing utilities.
%%
%% Copyright (c) 2018-20 Konnexions GmbH.  All Rights Reserved.
%%
%% -----------------------------------------------------------------------------

-ifndef(PLSQL_PARSER_FOLD_HRL).
-define(PLSQL_PARSER_FOLD_HRL, true).

-include("plsql_parser.hrl").

-define(CHAR_NEWLINE, case os:type() of
                          {unix, _} -> "\n";
                          _ -> "\r\n"
                      end).
-define(CHAR_TAB, "\t").

-define(CUSTOM_INIT(FunState, Ctx, PTree, FoldState),
    ?D("Start~n FunState: ~p~n CtxIn: ~p~n PTree: ~p~n FoldState: ~p~n",
        [FunState, Ctx, PTree, FoldState])).
-define(CUSTOM_RESULT(RT),
    ?D("~n CtxOut: ~p~n", [RT]),
    RT).

-define(FOLD_INIT(FunState, Ctx, PTree),
    ?D("Start~n FunState: ~p~n CtxIn: ~p~n PTree: ~p~n",
        [FunState, Ctx, PTree])).
-define(FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    (fun() ->
        _FunState = set_state_stmnt(FunStateIn, Rule),
        ?D("Start~n FunState: ~p~n CtxIn: ~p~n PTree: ~p~n",
            [_FunState, Ctx, PTree]),
        _FunState
     end)()).
-define(FOLD_RESULT(Ctx),
    ?D("~n CtxOut: ~p~n", [Ctx]),
    Ctx).

-record(fstate, {
    indent_lvl = 0,
    stmnts = []
}).

-endif.
