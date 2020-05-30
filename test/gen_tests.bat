@echo off
REM ----------------------------------------------------------------------------
REM
REM gen_tests.bat: PL/SQL - generating test data.
REM
REM Copyright (c) 2018-20 Konnexions GmbH.  All Rights Reserved.
REM
REM ----------------------------------------------------------------------------

> gen_tests.log (

    ECHO =======================================================================
    ECHO Start %0
    ECHO -----------------------------------------------------------------------
    ECHO Start Test Data Generation
    ECHO -----------------------------------------------------------------------
    ECHO:| TIME
    ECHO -----------------------------------------------------------------------

    IF EXIST _build\test\lib\plsql_parser\test\generated (
        RD /Q /S _build\test\lib\plsql_parser\test\generated
    )
    IF EXIST _build\test\lib\plsql_parser\test\performance_*.* (
        DEL /Q _build\test\lib\plsql_parser\test\performance_*.*
    )
    IF EXIST _build\test\lib\plsql_parser\test\reliability_*.* (
        DEL /Q _build\test\lib\plsql_parser\test\reliability_*.*
    )
    IF EXIST test\generated (
        RD /Q /S test\generated
    )

    CALL rebar3 as test compile

    REM Setting plsql_parser options ...........................................
    IF "%GENERATE_COMPACTED%" == "" (
        REM true: compacted / false: detailed.
        SET GENERATE_COMPACTED=true
        SET GENERATE_CT=true
        SET GENERATE_EUNIT=false
        SET GENERATE_PERFORMANCE=true
        SET GENERATE_RELIABILITY=true
        SET HEAP_SIZE=+hms 33554432
        SET LOGGING=false
        SET MAX_BASIC=500
    )

    REM Starting test data generator ...........................................
    erl -noshell -pa _build\test\lib\plsql_parser\test %HEAP_SIZE% -s plsql_parser_generator generate -s init stop

    IF EXIST code_templates (
        DIR code_templates
        DEL /Q code_templates
    )

    ECHO -----------------------------------------------------------------------
    ECHO:| TIME
    ECHO -----------------------------------------------------------------------
    ECHO End   %0
    ECHO =======================================================================

)
