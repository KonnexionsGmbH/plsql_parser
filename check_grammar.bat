@ECHO OFF
REM ----------------------------------------------------------------------------
REM
REM check_grammar.bat: PL/SQL - checking grammar definition with BNFC.
REM
REM Copyright (c) 2018-18 K2 Informatics GmbH.  All Rights Reserved.
REM
REM This file is provided to you under the Apache License,
REM Version 2.0 (the "License"); you may not use this file
REM except in compliance with the License.  You may obtain
REM a copy of the License at
REM
REM   http://www.apache.org/licenses/LICENSE-2.0
REM
REM Unless required by applicable law or agreed to in writing,
REM software distributed under the License is distributed on an
REM "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
REM KIND, either express or implied.  See the License for the
REM specific language governing permissions and limitations
REM under the License.
REM
REM ----------------------------------------------------------------------------

SETLOCAL enableDelayedExpansion

> check_grammar.log (

    ECHO ============================================================================
    ECHO !time! Start run
    ECHO ----------------------------------------------------------------------------

    IF EXIST tmp\* (
        DEL /Q tmp\*
    )

    bnfc -o tmp --haskell priv\bnf_converter\plsql_parser.cf

    happy -i tmp\ParPlsqlParser.y

    ECHO ----------------------------------------------------------------------------
    ECHO !time! End   run
    ECHO ============================================================================

)
