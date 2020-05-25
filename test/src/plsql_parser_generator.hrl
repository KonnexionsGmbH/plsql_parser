%% -----------------------------------------------------------------------------
%%
%% plsql_parser_generator.hrl: PL/SQL - test data generator.
%%
%% Copyright (c) 2018-20 Konnexions GmbH.  All Rights Reserved.
%%
%% -----------------------------------------------------------------------------

-ifndef(PLSQL_PARSER_GENERATOR_HRL).
-define(PLSQL_PARSER_GENERATOR_HRL, true).

-include("plsql_parser.hrl").

-define(ALIVE_COUNTER, 500).

-define(ALL_CLAUSE_PERFORMANCE, [
    createPackage
]).
-define(ALL_CLAUSE_RELIABILITY, [
    createPackage
]).
-define(ALL_CLAUSE_RELIABILITY_SQL, [
    createPackage
]).
-define(ALL_CLAUSE_RELIABILITY_SQL_DETAILED, [
%%    create_package
]).

-define(CHAR_NEWLINE, case os:type() of
                          {unix, _} -> "\n";
                          _ -> "\r\n"
                      end).

-define(CODE_TEMPLATES, code_templates).
-define(CREATE_CODE_END,
    [_CodeFirst | _] = Code,
    {_, _MemorySize} = erlang:process_info(self(), memory),
    ?D("~n code size          ===  ~12.. B rule: ~s ~n",
        [length(Code), atom_to_list(Rule)]),
    ?D("~n time (ms)          ===  ~12.. B rule: ~s ~n",
        [erlang:monotonic_time(1000) - _Start, atom_to_list(Rule)]),
    ?D("~n memory (bytes)     ===  ~12.. B rule: ~s ~n",
        [_MemorySize, atom_to_list(Rule)]),
    ?D("~n code size (bytes) <===  ~12.. B rule: ~s ~n",
        [length(_CodeFirst), atom_to_list(Rule)]),
    ok
).
-define(CREATE_CODE_START,
    [garbage_collect(Pid) || Pid <- processes()],
    _Start = erlang:monotonic_time(1000)
).

-define(F_RANDOM, fun(X, Y) -> erlang:phash2(X) < erlang:phash2(Y) end).

-define(GENERATE_COMPACTED, list_to_atom(string:to_lower(
    os:getenv("GENERATE_COMPACTED",
        "true")))).          % TRUE: compacted / FALSE: detailed.
-define(GENERATE_CT, list_to_atom(
    string:to_lower(os:getenv("GENERATE_CT", "true")))).
-define(GENERATE_EUNIT, list_to_atom(
    string:to_lower(os:getenv("GENERATE_EUNIT", "true")))).
-define(GENERATE_PERFORMANCE, list_to_atom(
    string:to_lower(os:getenv("GENERATE_PERFORMANCE", "true")))).
-define(GENERATE_RELIABILITY, list_to_atom(
    string:to_lower(os:getenv("GENERATE_RELIABILITY", "true")))).

-define(LOGGING, list_to_atom(string:to_lower(os:getenv("LOGGING", "false")))).

-define(MAX_BASIC, list_to_integer(os:getenv("MAX_BASIC", "250"))).
-define(MAX_STATEMENT, ?MAX_BASIC * 4).
-define(MAX_PLSQL, ?MAX_BASIC * 8).

-define(PATH_CT, "test/generated/ct/").
-define(PATH_EUNIT, "test/generated/eunit/").

-define(TIMETRAP_MINUTES, 15).

-endif.
