%% -----------------------------------------------------------------------------
%%
%% plsql_parser_dbss_test.hrl: SQL - DBSS format test driver.
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

-ifndef(PLSQL_PARSER_PRETTY_TEST_HRL).
-define(PLSQL_PARSER_PRETTY_TEST_HRL, true).

-include_lib("eunit/include/eunit.hrl").
-include("plsql_parser_fold.hrl").
-include("plsql_parser_test.hrl").

%%------------------------------------------------------------------------------
%% TEST 01 - Very simple.
%%------------------------------------------------------------------------------

-define(TEST_01, "
create or replace package dbss_flashback
is
    $if myString = \"myString\" $then
    function cre_restore_point_1 (
        p_is_show_in                   in char)
        return date;
    $else
    procedure cre_restore_point_2 (
        p_is_show_in                   in char);
    $end
end dbss_flashback;
/
").

-define(TEST_01_RESULT_DEFAULT, "<?xml version='1.0' encoding='UTF-8'?>
<Package xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' xsi:noNamespaceSchemaLocation='../../priv/dbss.xsd'>
    <Name>dbss_flashback</Name>
    <Function>
        <Name>cre_restore_point_1</Name>
        <Condition>
            <If>myString = \"myString\"</If>
        </Condition>
        <Parameter>
            <Name>p_is_show_in</Name>
            <Mode>IN</Mode>
            <DataType>CHAR</DataType>
        </Parameter>
        <Return>
            <DataType>DATE</DataType>
        </Return>
    </Function>
    <Procedure>
        <Name>cre_restore_point_2</Name>
        <Condition>
            <Else></Else>
        </Condition>
        <Parameter>
            <Name>p_is_show_in</Name>
            <Mode>IN</Mode>
            <DataType>CHAR</DataType>
        </Parameter>
    </Procedure>
</Package>").

%%------------------------------------------------------------------------------
%% TEST 02 - Simple.
%%------------------------------------------------------------------------------

-define(TEST_02, "
create or replace package dbss_flashback
is
    $if myString = \"myString\" $then
    --<> legacy_name_function = name_2_1
    function cre_restore_point_1 (
        --<> logger_to_character = false
        p_is_show_in                   in char)
        return date;
    $else
    --<> legacy_name_function = name_3_1
    --<> legacy_name_procedure = name_3_2
    procedure cre_restore_point_2 (
        p_is_show_in                   in char);
    $end
end dbss_flashback;
/
").

-define(TEST_02_RESULT_DEFAULT, "<?xml version='1.0' encoding='UTF-8'?>
<Package xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' xsi:noNamespaceSchemaLocation='../../priv/dbss.xsd'>
    <Name>dbss_flashback</Name>
    <Function>
        <Name>cre_restore_point_1</Name>
        <Condition>
            <If>myString = \"myString\"</If>
        </Condition>
        <LegacyNameFunction>name_2_1</LegacyNameFunction>
        <Parameter>
            <LoggerToCharacter>FALSE</LoggerToCharacter>
            <Name>p_is_show_in</Name>
            <Mode>IN</Mode>
            <DataType>CHAR</DataType>
        </Parameter>
        <Return>
            <DataType>DATE</DataType>
        </Return>
    </Function>
    <Procedure>
        <Name>cre_restore_point_2</Name>
        <Condition>
            <Else></Else>
        </Condition>
        <LegacyNameFunction>name_3_1</LegacyNameFunction>
        <LegacyNameProcedure>name_3_2</LegacyNameProcedure>
        <Parameter>
            <Name>p_is_show_in</Name>
            <Mode>IN</Mode>
            <DataType>CHAR</DataType>
        </Parameter>
    </Procedure>
</Package>").

%%------------------------------------------------------------------------------
%% TEST 03 - Default collation clause.
%%------------------------------------------------------------------------------

-define(TEST_03, "
create or replace package dbss_flashback
Default Collation Using_nls_comp
is
    procedure cre_restore_point_2 (
        p_is_show_in                   in char);
end dbss_flashback;
").

-define(TEST_03_RESULT_DEFAULT, "<?xml version='1.0' encoding='UTF-8'?>
<Package xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' xsi:noNamespaceSchemaLocation='../../priv/dbss.xsd'>
    <Name>dbss_flashback</Name>
    <Procedure>
        <Name>cre_restore_point_2</Name>
        <Parameter>
            <Name>p_is_show_in</Name>
            <Mode>IN</Mode>
            <DataType>CHAR</DataType>
        </Parameter>
    </Procedure>
</Package>").

%%------------------------------------------------------------------------------
%% TEST 04 - parallel_enabled_clause.
%%------------------------------------------------------------------------------

-define(TEST_04, "
create package namne_0
is
function name_1 (p_is_show_in char)
return date
Parallel_enabled ( Partition name_2 By Range (name_3) Cluster name_4 By (name_5))
;
end
;
").

-define(TEST_04_RESULT_DEFAULT, "<?xml version='1.0' encoding='UTF-8'?>
<Package xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' xsi:noNamespaceSchemaLocation='../../priv/dbss.xsd'>
    <Name>namne_0</Name>
    <Function>
        <Name>name_1</Name>
        <Parameter>
            <Name>p_is_show_in</Name>
            <Mode>IN</Mode>
            <DataType>CHAR</DataType>
        </Parameter>
        <Return>
            <DataType>DATE</DataType>
        </Return>
    </Function>
</Package>").

%%------------------------------------------------------------------------------
%% TEST 11 - Complete test.
%%------------------------------------------------------------------------------

-define(TEST_11, "
Create
Or Replace
Editionable
Package
package_schema.package_name
Sharing = Metadata
Accessible By accessor_name_11,
              accessor_schema_12.accessor_name_12,
              Package accessor_name_13
Default Collation Using_nls_comp
Authid Current_user
As
Function function_name_1 (parameter_name_01               Bfile,
                          parameter_name_02               Binary_double :=      expression_name_1,
                          parameter_name_03               Binary_float  Default expression_name_2,
                          parameter_name_04    Out        Binary_integer,
                          parameter_name_05    Out Nocopy Blob,
                          parameter_name_06 In            Boolean,
                          parameter_name_07 In            Char :=         expression_name_3 = expression_name_4,
                          parameter_name_08 In            Char(5) Default expression_name_5,
                          parameter_name_09 In Out        Char(6 Byte),
                          parameter_name_10 In Out Nocopy Char(7 Char))
         Return Clob
         Accessible By accessor_name_21,
                       accessor_schema_22.accessor_name_22,
                       Package accessor_name_23
         Deterministic
         Parallel_enabled ( Partition partition_name_1 By Range (
                                                                  column_name_1,
                                                                  column_name_2(+),
                                                                  table_name_3.*,
                                                                  table_name_4.column_name_4,
                                                                  table_name_5.column_name_5(+),
                                                                  schema_name_6.table_name_6.*,
                                                                  schema_name_7.Table_name_7.column_name_7,
                                                                  schema_name_8.Table_name_8.column_name_8(+)
                                                                )
                                                                Cluster expression_name_1 || expression_name_2 By
                                                                (
                                                                  column_name_9,
                                                                  column_name_10
                                                                )
                          )
         Pipelined Table Polymorphic Using schema_name_1.table_name_1
         Result_cache Relies_on (
                                 table_name_1,
                                 schema_name_1.table_name_1
                                )
         ;
Procedure procedure_name_1 (
                            parameter_name_01               Date,
                            parameter_name_02               Float      :=      function_name_1 (
                                                                                                parameter_name_1,
                                                                                                parameter_name_2,
                                                                                                parameter_name_3 + parameter_name_4,
                                                                                                parameter_name_5,
                                                                                                function_name_1(
                                                                                                                parameter_name_1,
                                                                                                                parameter_name_2,
                                                                                                                parameter_name_3 + parameter_name_4,
                                                                                                                parameter_name_5,
                                                                                                                function_name_2(),
                                                                                                                \"string_1\",
                                                                                                                null,
                                                                                                                :parameter_1,
                                                                                                                - column_name_1
                                                                                                               ),
                                                                                                \"string_2\",
                                                                                                null,
                                                                                                :parameter_2 Indicator :parameter_3,
                                                                                                + column_name_1
                                                                                               ),
                            parameter_name_03               Float (10) Default null,
                            parameter_name_04    Out        Interval Day To Second,
                            parameter_name_05    Out Nocopy Interval Day To Second(11),
                            parameter_name_06 In            Interval Day (12) To Second,
                            parameter_name_07 In            Interval Day (13) To Second (14) :=      \"string_3\",
                            parameter_name_08 In            Interval Year To Month           Default +5,
                            parameter_name_09 In Out        Interval Year (15) To Month,
                            parameter_name_10 In Out Nocopy Char(7 Char)
                           )
          Accessible By accessor_name_31,
                        accessor_schema_32.accessor_name_32,
                        Package accessor_name_33
          ;
End package_name
;
/
").

-define(TEST_11_RESULT_DEFAULT, "<?xml version='1.0' encoding='UTF-8'?>
<Package xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' xsi:noNamespaceSchemaLocation='../../priv/dbss.xsd'>
    <Name>package_schema.package_name</Name>
    <Function>
        <Name>function_name_1</Name>
        <Parameter>
            <Name>parameter_name_01</Name>
            <Mode>IN</Mode>
            <DataType>BFILE</DataType>
        </Parameter>
        <Parameter>
            <Name>parameter_name_02</Name>
            <Mode>IN</Mode>
            <DataType>BINARY_DOUBLE</DataType>
            <DefaultValue>
                <Expression>expression_name_1</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>parameter_name_03</Name>
            <Mode>IN</Mode>
            <DataType>BINARY_FLOAT</DataType>
            <DefaultValue>
                <Expression>expression_name_2</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>parameter_name_04</Name>
            <Mode>OUT</Mode>
            <DataType>BINARY_INTEGER</DataType>
        </Parameter>
        <Parameter>
            <Name>parameter_name_05</Name>
            <Mode>OUT NOCOPY</Mode>
            <DataType>BLOB</DataType>
        </Parameter>
        <Parameter>
            <Name>parameter_name_06</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
        </Parameter>
        <Parameter>
            <Name>parameter_name_07</Name>
            <Mode>IN</Mode>
            <DataType>CHAR</DataType>
            <DefaultValue>
                <Expression>expression_name_3 = expression_name_4</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>parameter_name_08</Name>
            <Mode>IN</Mode>
            <DataType>CHAR(5)</DataType>
            <DefaultValue>
                <Expression>expression_name_5</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>parameter_name_09</Name>
            <Mode>IN OUT</Mode>
            <DataType>CHAR(6 BYTE)</DataType>
        </Parameter>
        <Parameter>
            <Name>parameter_name_10</Name>
            <Mode>IN OUT NOCOPY</Mode>
            <DataType>CHAR(7 CHAR)</DataType>
        </Parameter>
        <Return>
            <DataType>CLOB</DataType>
        </Return>
 || ()    </Function>
    <Procedure>
        <Name>procedure_name_1</Name>
        <Parameter>
            <Name>parameter_name_01</Name>
            <Mode>IN</Mode>
            <DataType>DATE</DataType>
        </Parameter>
        <Parameter>
            <Name>parameter_name_02</Name>
            <Mode>IN</Mode>
            <DataType>FLOAT</DataType>
            <DefaultValue>
                <Expression>function_name_1(parameter_name_1parameter_name_2parameter_name_3 + parameter_name_4parameter_name_5function_name_1(parameter_name_1parameter_name_2parameter_name_3 + parameter_name_4parameter_name_5function_name_2()\"string_1\"NULL:parameter_1-column_name_1)\"string_2\"NULL:parameter_2 indicator :parameter_3+column_name_1)</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>parameter_name_03</Name>
            <Mode>IN</Mode>
            <DataType>FLOAT(10)</DataType>
            <DefaultValue>
                <Expression>NULL</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>parameter_name_04</Name>
            <Mode>OUT</Mode>
            <DataType>INTERVAL DAY to second</DataType>
        </Parameter>
        <Parameter>
            <Name>parameter_name_05</Name>
            <Mode>OUT NOCOPY</Mode>
            <DataType>INTERVAL DAY to second(11)</DataType>
        </Parameter>
        <Parameter>
            <Name>parameter_name_06</Name>
            <Mode>IN</Mode>
            <DataType>INTERVAL DAY(12) to second</DataType>
        </Parameter>
        <Parameter>
            <Name>parameter_name_07</Name>
            <Mode>IN</Mode>
            <DataType>INTERVAL DAY(13) to second(14)</DataType>
            <DefaultValue>
                <Expression>\"string_3\"</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>parameter_name_08</Name>
            <Mode>IN</Mode>
            <DataType>INTERVAL YEAR to month</DataType>
            <DefaultValue>
                <Expression>+5</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>parameter_name_09</Name>
            <Mode>IN OUT</Mode>
            <DataType>INTERVAL YEAR(15) to month</DataType>
        </Parameter>
        <Parameter>
            <Name>parameter_name_10</Name>
            <Mode>IN OUT NOCOPY</Mode>
            <DataType>CHAR(7 CHAR)</DataType>
        </Parameter>
    </Procedure>
</Package>").

%%------------------------------------------------------------------------------
%% TEST 12 - Real world example I.
%%------------------------------------------------------------------------------

-define(TEST_12, "
CREATE OR REPLACE PACKAGE wwe.dbss_flashback
    AUTHID CURRENT_USER
IS
    /*
    <VERSION>
       1.0.0
    </VERSION>

    <FILENAME>
       dbss_flashback.pks
    </FILENAME>

    <AUTHOR>
       K2 Infornatics GmbH
    </AUTHOR>

    <SUMMARY>
       Execute DBSS flashback tasks.
    </SUMMARY>

    <COPYRIGHT>
       Swisscom AG
    </COPYRIGHT>

    <YEARS>
       2018
    </YEARS>

    <OVERVIEW>
       Oracle Flashback Technology is a group of Oracle Database features that
       let you view past states of database objects or to return database
       objects to a previous state without using point-in-time media recovery.
    </OVERVIEW>

    <DEPENDENCIES>
       None
    </DEPENDENCIES>

    <EXCEPTIONS>
       None
    </EXCEPTIONS>

    Modification History

    Date       By        Modification
    ---------- --------- ------------------------------------
    <MODIFICATIONS>
    08/05/2018 WW        Package created.
    </MODIFICATIONS>
    */

    /* =========================================================================
       Public Procedure Declaration.
       ---------------------------------------------------------------------- */

    $IF DBMS_DB_VERSION.version = 12 AND DBMS_DB_VERSION.release = 1
    $THEN
        --<> legacy_name_function = dbss_f_cre_rstr_pnt_12_1
        --<> legacy_name_procedure = dbss_p_cre_rstr_pnt_12_1
        PROCEDURE cre_rstr_pnt (
            p_sqls_cv_in_out               IN OUT dbss_type.sqls_cur,
            p_rstr_pnt_in                  IN     v$restore_point.name%TYPE := dbss_global.get_default_rstr_pnt (),
            p_scn_in                       IN     NUMBER := 0,
            p_is_cursor_in                 IN     BOOLEAN := dbss_global.get_default_is_cursor (),
            p_is_execute_in                IN     BOOLEAN := dbss_global.get_default_is_execute (),
            p_is_show_in                   IN     BOOLEAN := dbss_global.get_default_is_show ());
    $END

    $IF DBMS_DB_VERSION.version > 12 OR DBMS_DB_VERSION.version = 12 AND DBMS_DB_VERSION.release > 1
    $THEN
        --<> legacy_name_function = dbss_f_cre_rstr_pnt_12_2
        --<> legacy_name_procedure = dbss_p_cre_rstr_pnt_12_2
        PROCEDURE cre_rstr_pnt (
            p_sqls_cv_in_out               IN OUT dbss_type.sqls_cur,
            p_rstr_pnt_in                  IN     v$restore_point.name%TYPE := dbss_global.get_default_rstr_pnt (),
            p_is_clean_in                  IN     BOOLEAN := FALSE,
            p_scn_in                       IN     NUMBER := 0,
            p_is_cursor_in                 IN     BOOLEAN := dbss_global.get_default_is_cursor (),
            p_is_execute_in                IN     BOOLEAN := dbss_global.get_default_is_execute (),
            p_is_show_in                   IN     BOOLEAN := dbss_global.get_default_is_show ());
    $END

    $IF DBMS_DB_VERSION.version = 12 AND DBMS_DB_VERSION.release = 1
    $THEN
        --<> legacy_name_function = dbss_f_cre_rstr_pnt_guar_12_1
        --<> legacy_name_procedure = dbss_p_cre_rstr_pnt_guar_12_1
        PROCEDURE cre_rstr_pnt_guar (
            p_sqls_cv_in_out               IN OUT dbss_type.sqls_cur,
            p_rstr_pnt_in                  IN     v$restore_point.name%TYPE := dbss_global.get_default_rstr_pnt (),
            p_scn_in                       IN     NUMBER := 0,
            p_is_cursor_in                 IN     BOOLEAN := dbss_global.get_default_is_cursor (),
            p_is_execute_in                IN     BOOLEAN := dbss_global.get_default_is_execute (),
            p_is_show_in                   IN     BOOLEAN := dbss_global.get_default_is_show ());
    $END

    $IF DBMS_DB_VERSION.version > 12 OR DBMS_DB_VERSION.version = 12 AND DBMS_DB_VERSION.release > 1
    $THEN
        --<> legacy_name_function = dbss_f_cre_rstr_pnt_guar_12_2
        --<> legacy_name_procedure = dbss_p_cre_rstr_pnt_guar_12_2
        PROCEDURE cre_rstr_pnt_guar (
            p_sqls_cv_in_out               IN OUT dbss_type.sqls_cur,
            p_rstr_pnt_in                  IN     v$restore_point.name%TYPE := dbss_global.get_default_rstr_pnt (),
            p_is_clean_in                  IN     BOOLEAN := FALSE,
            p_scn_in                       IN     NUMBER := 0,
            p_is_cursor_in                 IN     BOOLEAN := dbss_global.get_default_is_cursor (),
            p_is_execute_in                IN     BOOLEAN := dbss_global.get_default_is_execute (),
            p_is_show_in                   IN     BOOLEAN := dbss_global.get_default_is_show ());
    $END

    $IF DBMS_DB_VERSION.version = 12 AND DBMS_DB_VERSION.release = 1
    $THEN
        --<> legacy_name_function = dbss_f_cre_rstr_pnt_pres_12_1
        --<> legacy_name_procedure = dbss_p_cre_rstr_pnt_pres_12_1
        PROCEDURE cre_rstr_pnt_pres (
            p_sqls_cv_in_out               IN OUT dbss_type.sqls_cur,
            p_rstr_pnt_in                  IN     v$restore_point.name%TYPE := dbss_global.get_default_rstr_pnt (),
            p_scn_in                       IN     NUMBER := 0,
            p_is_cursor_in                 IN     BOOLEAN := dbss_global.get_default_is_cursor (),
            p_is_execute_in                IN     BOOLEAN := dbss_global.get_default_is_execute (),
            p_is_show_in                   IN     BOOLEAN := dbss_global.get_default_is_show ());
    $END

    $IF DBMS_DB_VERSION.version > 12 OR DBMS_DB_VERSION.version = 12 AND DBMS_DB_VERSION.release > 1
    $THEN
        --<> legacy_name_function = dbss_f_cre_rstr_pnt_pres_12_2
        --<> legacy_name_procedure = dbss_p_cre_rstr_pnt_pres_12_2
        PROCEDURE cre_rstr_pnt_pres (
            p_sqls_cv_in_out               IN OUT dbss_type.sqls_cur,
            p_rstr_pnt_in                  IN     v$restore_point.name%TYPE := dbss_global.get_default_rstr_pnt (),
            p_is_clean_in                  IN     BOOLEAN := FALSE,
            p_scn_in                       IN     NUMBER := 0,
            p_is_cursor_in                 IN     BOOLEAN := dbss_global.get_default_is_cursor (),
            p_is_execute_in                IN     BOOLEAN := dbss_global.get_default_is_execute (),
            p_is_show_in                   IN     BOOLEAN := dbss_global.get_default_is_show ());
    $END

    PROCEDURE del_rstr_pnt (
        p_sqls_cv_in_out               IN OUT dbss_type.sqls_cur,
        p_rstr_pnt_in                  IN     v$restore_point.name%TYPE := dbss_global.get_default_rstr_pnt (),
        p_is_cursor_in                 IN     BOOLEAN := dbss_global.get_default_is_cursor (),
        p_is_execute_in                IN     BOOLEAN := dbss_global.get_default_is_execute (),
        p_is_show_in                   IN     BOOLEAN := dbss_global.get_default_is_show ());

    PROCEDURE del_rstr_pnts (
        p_sqls_cv_in_out               IN OUT dbss_type.sqls_cur,
        p_is_cursor_in                 IN     BOOLEAN := dbss_global.get_default_is_cursor (),
        p_is_execute_in                IN     BOOLEAN := dbss_global.get_default_is_execute (),
        p_is_show_in                   IN     BOOLEAN := dbss_global.get_default_is_show ());

    PROCEDURE del_rstr_pnts_guar (
        p_sqls_cv_in_out               IN OUT dbss_type.sqls_cur,
        p_is_cursor_in                 IN     BOOLEAN := dbss_global.get_default_is_cursor (),
        p_is_execute_in                IN     BOOLEAN := dbss_global.get_default_is_execute (),
        p_is_show_in                   IN     BOOLEAN := dbss_global.get_default_is_show ());

    PROCEDURE del_rstr_pnts_norm (
        p_sqls_cv_in_out               IN OUT dbss_type.sqls_cur,
        p_is_cursor_in                 IN     BOOLEAN := dbss_global.get_default_is_cursor (),
        p_is_execute_in                IN     BOOLEAN := dbss_global.get_default_is_execute (),
        p_is_show_in                   IN     BOOLEAN := dbss_global.get_default_is_show ());

    PROCEDURE del_rstr_pnts_pres (
        p_sqls_cv_in_out               IN OUT dbss_type.sqls_cur,
        p_is_cursor_in                 IN     BOOLEAN := dbss_global.get_default_is_cursor (),
        p_is_execute_in                IN     BOOLEAN := dbss_global.get_default_is_execute (),
        p_is_show_in                   IN     BOOLEAN := dbss_global.get_default_is_show ());

    $IF DBMS_DB_VERSION.version = 12 AND DBMS_DB_VERSION.release = 1
    $THEN
        --<> legacy_name_function = dbss_f_get_rstr_pnts_12_1
        --<> legacy_name_procedure = dbss_p_get_rstr_pnts_12_1
        PROCEDURE get_rstr_pnts (
            p_rstr_pnts_cv_out                OUT dbss_type.rstr_pnts_cur,
            p_guaranteed_in                IN     v$restore_point.guarantee_flashback_database%TYPE := 'ALL',
            p_preserved_in                 IN     v$restore_point.preserved%TYPE := 'ALL',
            p_is_cursor_in                 IN     BOOLEAN := dbss_global.get_default_is_cursor (),
            p_is_show_in                   IN     BOOLEAN := dbss_global.get_default_is_show ());
    $END

    $IF DBMS_DB_VERSION.version > 12 OR DBMS_DB_VERSION.version = 12 AND DBMS_DB_VERSION.release > 1
    $THEN
        --<> legacy_name_function = dbss_f_get_rstr_pnts_12_2
        --<> legacy_name_procedure = dbss_p_get_rstr_pnts_12_2
        PROCEDURE get_rstr_pnts (
            p_rstr_pnts_cv_out                OUT dbss_type.rstr_pnts_cur,
            p_clean_in                     IN     v$restore_point.clean_pdb_restore_point%TYPE := 'ALL',
            p_guaranteed_in                IN     v$restore_point.guarantee_flashback_database%TYPE := 'ALL',
            p_preserved_in                 IN     v$restore_point.preserved%TYPE := 'ALL',
            p_is_cursor_in                 IN     BOOLEAN := dbss_global.get_default_is_cursor (),
            p_is_show_in                   IN     BOOLEAN := dbss_global.get_default_is_show ());
    $END

    PROCEDURE set_flashback_off (
        p_sqls_cv_in_out               IN OUT dbss_type.sqls_cur,
        p_is_cursor_in                 IN     BOOLEAN := dbss_global.get_default_is_cursor (),
        p_is_execute_in                IN     BOOLEAN := dbss_global.get_default_is_execute (),
        p_is_show_in                   IN     BOOLEAN := dbss_global.get_default_is_show ());

    PROCEDURE set_flashback_on (
        p_sqls_cv_in_out               IN OUT dbss_type.sqls_cur,
        p_is_cursor_in                 IN     BOOLEAN := dbss_global.get_default_is_cursor (),
        p_is_execute_in                IN     BOOLEAN := dbss_global.get_default_is_execute (),
        p_is_show_in                   IN     BOOLEAN := dbss_global.get_default_is_show ());
END dbss_flashback;
/
").

-define(TEST_12_RESULT_DEFAULT, "<?xml version='1.0' encoding='UTF-8'?>
<Package xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' xsi:noNamespaceSchemaLocation='../../priv/dbss.xsd'>
    <Name>wwe.dbss_flashback</Name>
    <Procedure>
        <Name>cre_rstr_pnt</Name>
        <Condition>
            <IfEnd>DBMS_DB_VERSION.version = 12 AND DBMS_DB_VERSION.release = 1</IfEnd>
        </Condition>
        <LegacyNameFunction>dbss_f_cre_rstr_pnt_12_1</LegacyNameFunction>
        <LegacyNameProcedure>dbss_p_cre_rstr_pnt_12_1</LegacyNameProcedure>
        <Parameter>
            <Name>p_sqls_cv_in_out</Name>
            <Mode>IN OUT</Mode>
            <DataType>dbss_type.sqls_cur</DataType>
        </Parameter>
        <Parameter>
            <Name>p_rstr_pnt_in</Name>
            <Mode>IN</Mode>
            <DataType>v$restore_point.NAME%TYPE</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_rstr_pnt()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_scn_in</Name>
            <Mode>IN</Mode>
            <DataType>NUMBER</DataType>
            <DefaultValue>
                <Expression>0</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_cursor_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_cursor()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_execute_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_execute()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_show_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_show()</Expression>
            </DefaultValue>
        </Parameter>
    </Procedure>
    <Procedure>
        <Name>cre_rstr_pnt</Name>
        <Condition>
            <IfEnd>DBMS_DB_VERSION.version > 12 OR DBMS_DB_VERSION.version = 12 AND DBMS_DB_VERSION.release > 1</IfEnd>
        </Condition>
        <LegacyNameFunction>dbss_f_cre_rstr_pnt_12_2</LegacyNameFunction>
        <LegacyNameProcedure>dbss_p_cre_rstr_pnt_12_2</LegacyNameProcedure>
        <Parameter>
            <Name>p_sqls_cv_in_out</Name>
            <Mode>IN OUT</Mode>
            <DataType>dbss_type.sqls_cur</DataType>
        </Parameter>
        <Parameter>
            <Name>p_rstr_pnt_in</Name>
            <Mode>IN</Mode>
            <DataType>v$restore_point.NAME%TYPE</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_rstr_pnt()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_clean_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>FALSE</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_scn_in</Name>
            <Mode>IN</Mode>
            <DataType>NUMBER</DataType>
            <DefaultValue>
                <Expression>0</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_cursor_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_cursor()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_execute_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_execute()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_show_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_show()</Expression>
            </DefaultValue>
        </Parameter>
    </Procedure>
    <Procedure>
        <Name>cre_rstr_pnt_guar</Name>
        <Condition>
            <IfEnd>DBMS_DB_VERSION.version = 12 AND DBMS_DB_VERSION.release = 1</IfEnd>
        </Condition>
        <LegacyNameFunction>dbss_f_cre_rstr_pnt_guar_12_1</LegacyNameFunction>
        <LegacyNameProcedure>dbss_p_cre_rstr_pnt_guar_12_1</LegacyNameProcedure>
        <Parameter>
            <Name>p_sqls_cv_in_out</Name>
            <Mode>IN OUT</Mode>
            <DataType>dbss_type.sqls_cur</DataType>
        </Parameter>
        <Parameter>
            <Name>p_rstr_pnt_in</Name>
            <Mode>IN</Mode>
            <DataType>v$restore_point.NAME%TYPE</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_rstr_pnt()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_scn_in</Name>
            <Mode>IN</Mode>
            <DataType>NUMBER</DataType>
            <DefaultValue>
                <Expression>0</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_cursor_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_cursor()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_execute_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_execute()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_show_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_show()</Expression>
            </DefaultValue>
        </Parameter>
    </Procedure>
    <Procedure>
        <Name>cre_rstr_pnt_guar</Name>
        <Condition>
            <IfEnd>DBMS_DB_VERSION.version > 12 OR DBMS_DB_VERSION.version = 12 AND DBMS_DB_VERSION.release > 1</IfEnd>
        </Condition>
        <LegacyNameFunction>dbss_f_cre_rstr_pnt_guar_12_2</LegacyNameFunction>
        <LegacyNameProcedure>dbss_p_cre_rstr_pnt_guar_12_2</LegacyNameProcedure>
        <Parameter>
            <Name>p_sqls_cv_in_out</Name>
            <Mode>IN OUT</Mode>
            <DataType>dbss_type.sqls_cur</DataType>
        </Parameter>
        <Parameter>
            <Name>p_rstr_pnt_in</Name>
            <Mode>IN</Mode>
            <DataType>v$restore_point.NAME%TYPE</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_rstr_pnt()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_clean_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>FALSE</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_scn_in</Name>
            <Mode>IN</Mode>
            <DataType>NUMBER</DataType>
            <DefaultValue>
                <Expression>0</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_cursor_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_cursor()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_execute_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_execute()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_show_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_show()</Expression>
            </DefaultValue>
        </Parameter>
    </Procedure>
    <Procedure>
        <Name>cre_rstr_pnt_pres</Name>
        <Condition>
            <IfEnd>DBMS_DB_VERSION.version = 12 AND DBMS_DB_VERSION.release = 1</IfEnd>
        </Condition>
        <LegacyNameFunction>dbss_f_cre_rstr_pnt_pres_12_1</LegacyNameFunction>
        <LegacyNameProcedure>dbss_p_cre_rstr_pnt_pres_12_1</LegacyNameProcedure>
        <Parameter>
            <Name>p_sqls_cv_in_out</Name>
            <Mode>IN OUT</Mode>
            <DataType>dbss_type.sqls_cur</DataType>
        </Parameter>
        <Parameter>
            <Name>p_rstr_pnt_in</Name>
            <Mode>IN</Mode>
            <DataType>v$restore_point.NAME%TYPE</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_rstr_pnt()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_scn_in</Name>
            <Mode>IN</Mode>
            <DataType>NUMBER</DataType>
            <DefaultValue>
                <Expression>0</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_cursor_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_cursor()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_execute_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_execute()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_show_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_show()</Expression>
            </DefaultValue>
        </Parameter>
    </Procedure>
    <Procedure>
        <Name>cre_rstr_pnt_pres</Name>
        <Condition>
            <IfEnd>DBMS_DB_VERSION.version > 12 OR DBMS_DB_VERSION.version = 12 AND DBMS_DB_VERSION.release > 1</IfEnd>
        </Condition>
        <LegacyNameFunction>dbss_f_cre_rstr_pnt_pres_12_2</LegacyNameFunction>
        <LegacyNameProcedure>dbss_p_cre_rstr_pnt_pres_12_2</LegacyNameProcedure>
        <Parameter>
            <Name>p_sqls_cv_in_out</Name>
            <Mode>IN OUT</Mode>
            <DataType>dbss_type.sqls_cur</DataType>
        </Parameter>
        <Parameter>
            <Name>p_rstr_pnt_in</Name>
            <Mode>IN</Mode>
            <DataType>v$restore_point.NAME%TYPE</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_rstr_pnt()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_clean_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>FALSE</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_scn_in</Name>
            <Mode>IN</Mode>
            <DataType>NUMBER</DataType>
            <DefaultValue>
                <Expression>0</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_cursor_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_cursor()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_execute_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_execute()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_show_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_show()</Expression>
            </DefaultValue>
        </Parameter>
    </Procedure>
    <Procedure>
        <Name>del_rstr_pnt</Name>
        <Parameter>
            <Name>p_sqls_cv_in_out</Name>
            <Mode>IN OUT</Mode>
            <DataType>dbss_type.sqls_cur</DataType>
        </Parameter>
        <Parameter>
            <Name>p_rstr_pnt_in</Name>
            <Mode>IN</Mode>
            <DataType>v$restore_point.NAME%TYPE</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_rstr_pnt()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_cursor_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_cursor()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_execute_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_execute()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_show_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_show()</Expression>
            </DefaultValue>
        </Parameter>
    </Procedure>
    <Procedure>
        <Name>del_rstr_pnts</Name>
        <Parameter>
            <Name>p_sqls_cv_in_out</Name>
            <Mode>IN OUT</Mode>
            <DataType>dbss_type.sqls_cur</DataType>
        </Parameter>
        <Parameter>
            <Name>p_is_cursor_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_cursor()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_execute_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_execute()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_show_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_show()</Expression>
            </DefaultValue>
        </Parameter>
    </Procedure>
    <Procedure>
        <Name>del_rstr_pnts_guar</Name>
        <Parameter>
            <Name>p_sqls_cv_in_out</Name>
            <Mode>IN OUT</Mode>
            <DataType>dbss_type.sqls_cur</DataType>
        </Parameter>
        <Parameter>
            <Name>p_is_cursor_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_cursor()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_execute_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_execute()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_show_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_show()</Expression>
            </DefaultValue>
        </Parameter>
    </Procedure>
    <Procedure>
        <Name>del_rstr_pnts_norm</Name>
        <Parameter>
            <Name>p_sqls_cv_in_out</Name>
            <Mode>IN OUT</Mode>
            <DataType>dbss_type.sqls_cur</DataType>
        </Parameter>
        <Parameter>
            <Name>p_is_cursor_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_cursor()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_execute_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_execute()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_show_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_show()</Expression>
            </DefaultValue>
        </Parameter>
    </Procedure>
    <Procedure>
        <Name>del_rstr_pnts_pres</Name>
        <Parameter>
            <Name>p_sqls_cv_in_out</Name>
            <Mode>IN OUT</Mode>
            <DataType>dbss_type.sqls_cur</DataType>
        </Parameter>
        <Parameter>
            <Name>p_is_cursor_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_cursor()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_execute_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_execute()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_show_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_show()</Expression>
            </DefaultValue>
        </Parameter>
    </Procedure>
    <Procedure>
        <Name>get_rstr_pnts</Name>
        <Condition>
            <IfEnd>DBMS_DB_VERSION.version = 12 AND DBMS_DB_VERSION.release = 1</IfEnd>
        </Condition>
        <LegacyNameFunction>dbss_f_get_rstr_pnts_12_1</LegacyNameFunction>
        <LegacyNameProcedure>dbss_p_get_rstr_pnts_12_1</LegacyNameProcedure>
        <Parameter>
            <Name>p_rstr_pnts_cv_out</Name>
            <Mode>OUT</Mode>
            <DataType>dbss_type.rstr_pnts_cur</DataType>
        </Parameter>
        <Parameter>
            <Name>p_guaranteed_in</Name>
            <Mode>IN</Mode>
            <DataType>v$restore_point.guarantee_flashback_database%TYPE</DataType>
            <DefaultValue>
                <Expression>'ALL'</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_preserved_in</Name>
            <Mode>IN</Mode>
            <DataType>v$restore_point.preserved%TYPE</DataType>
            <DefaultValue>
                <Expression>'ALL'</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_cursor_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_cursor()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_show_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_show()</Expression>
            </DefaultValue>
        </Parameter>
    </Procedure>
    <Procedure>
        <Name>get_rstr_pnts</Name>
        <Condition>
            <IfEnd>DBMS_DB_VERSION.version > 12 OR DBMS_DB_VERSION.version = 12 AND DBMS_DB_VERSION.release > 1</IfEnd>
        </Condition>
        <LegacyNameFunction>dbss_f_get_rstr_pnts_12_2</LegacyNameFunction>
        <LegacyNameProcedure>dbss_p_get_rstr_pnts_12_2</LegacyNameProcedure>
        <Parameter>
            <Name>p_rstr_pnts_cv_out</Name>
            <Mode>OUT</Mode>
            <DataType>dbss_type.rstr_pnts_cur</DataType>
        </Parameter>
        <Parameter>
            <Name>p_clean_in</Name>
            <Mode>IN</Mode>
            <DataType>v$restore_point.clean_pdb_restore_point%TYPE</DataType>
            <DefaultValue>
                <Expression>'ALL'</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_guaranteed_in</Name>
            <Mode>IN</Mode>
            <DataType>v$restore_point.guarantee_flashback_database%TYPE</DataType>
            <DefaultValue>
                <Expression>'ALL'</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_preserved_in</Name>
            <Mode>IN</Mode>
            <DataType>v$restore_point.preserved%TYPE</DataType>
            <DefaultValue>
                <Expression>'ALL'</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_cursor_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_cursor()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_show_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_show()</Expression>
            </DefaultValue>
        </Parameter>
    </Procedure>
    <Procedure>
        <Name>set_flashback_off</Name>
        <Parameter>
            <Name>p_sqls_cv_in_out</Name>
            <Mode>IN OUT</Mode>
            <DataType>dbss_type.sqls_cur</DataType>
        </Parameter>
        <Parameter>
            <Name>p_is_cursor_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_cursor()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_execute_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_execute()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_show_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_show()</Expression>
            </DefaultValue>
        </Parameter>
    </Procedure>
    <Procedure>
        <Name>set_flashback_on</Name>
        <Parameter>
            <Name>p_sqls_cv_in_out</Name>
            <Mode>IN OUT</Mode>
            <DataType>dbss_type.sqls_cur</DataType>
        </Parameter>
        <Parameter>
            <Name>p_is_cursor_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_cursor()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_execute_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_execute()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_show_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_show()</Expression>
            </DefaultValue>
        </Parameter>
    </Procedure>
</Package>").

%%------------------------------------------------------------------------------
%% TEST 12 - Real world example II.
%%------------------------------------------------------------------------------

-define(TEST_13, "
CREATE OR REPLACE PACKAGE wwe.dbss_flashback
    AUTHID CURRENT_USER
IS
    /*
    <VERSION>
       1.0.0
    </VERSION>

    <FILENAME>
       dbss_flashback.pks
    </FILENAME>

    <AUTHOR>
       K2 Infornatics GmbH
    </AUTHOR>

    <SUMMARY>
       Execute DBSS flashback tasks.
    </SUMMARY>

    <COPYRIGHT>
       Swisscom AG
    </COPYRIGHT>

    <YEARS>
       2018
    </YEARS>

    <OVERVIEW>
       Oracle Flashback Technology is a group of Oracle Database features that
       let you view past states of database objects or to return database
       objects to a previous state without using point-in-time media recovery.
    </OVERVIEW>

    <DEPENDENCIES>
       None
    </DEPENDENCIES>

    <EXCEPTIONS>
       None
    </EXCEPTIONS>

    Modification History

    Date       By        Modification
    ---------- --------- ------------------------------------
    <MODIFICATIONS>
    08/05/2018 WW        Package created.
    </MODIFICATIONS>
    */

    /* =========================================================================
       Public Procedure Declaration.
       ---------------------------------------------------------------------- */

    $IF DBMS_DB_VERSION.version = 12 AND DBMS_DB_VERSION.release = 1
    $THEN
        --<> system_privilege = flashback any table
        PROCEDURE cre_rstr_pnt (
            p_sqls_cv_in_out               IN OUT dbss_type.sqls_cur,
            p_rstr_pnt_in                  IN     v$restore_point.name%TYPE := dbss_global.get_default_rstr_pnt (),
            p_scn_in                       IN     NUMBER := 0,
            p_is_cursor_in                 IN     BOOLEAN := dbss_global.get_default_is_cursor (),
            p_is_execute_in                IN     BOOLEAN := dbss_global.get_default_is_execute (),
            p_is_show_in                   IN     BOOLEAN := dbss_global.get_default_is_show ());
    $END

    $IF DBMS_DB_VERSION.version > 12 OR DBMS_DB_VERSION.version = 12 AND DBMS_DB_VERSION.release > 1
    $THEN
        --<> system_privilege = flashback any table
        PROCEDURE cre_rstr_pnt (
            p_sqls_cv_in_out               IN OUT dbss_type.sqls_cur,
            p_rstr_pnt_in                  IN     v$restore_point.name%TYPE := dbss_global.get_default_rstr_pnt (),
            p_is_clean_in                  IN     BOOLEAN := FALSE,
            p_scn_in                       IN     NUMBER := 0,
            p_is_cursor_in                 IN     BOOLEAN := dbss_global.get_default_is_cursor (),
            p_is_execute_in                IN     BOOLEAN := dbss_global.get_default_is_execute (),
            p_is_show_in                   IN     BOOLEAN := dbss_global.get_default_is_show ());
    $END

    $IF DBMS_DB_VERSION.version = 12 AND DBMS_DB_VERSION.release = 1
    $THEN
        --<> system_privilege = sysdba
        PROCEDURE cre_rstr_pnt_guar (
            p_sqls_cv_in_out               IN OUT dbss_type.sqls_cur,
            p_rstr_pnt_in                  IN     v$restore_point.name%TYPE := dbss_global.get_default_rstr_pnt (),
            p_scn_in                       IN     NUMBER := 0,
            p_is_cursor_in                 IN     BOOLEAN := dbss_global.get_default_is_cursor (),
            p_is_execute_in                IN     BOOLEAN := dbss_global.get_default_is_execute (),
            p_is_show_in                   IN     BOOLEAN := dbss_global.get_default_is_show ());
    $END

    $IF DBMS_DB_VERSION.version > 12 OR DBMS_DB_VERSION.version = 12 AND DBMS_DB_VERSION.release > 1
    $THEN
        --<> system_privilege = sysdba
        PROCEDURE cre_rstr_pnt_guar (
            p_sqls_cv_in_out               IN OUT dbss_type.sqls_cur,
            p_rstr_pnt_in                  IN     v$restore_point.name%TYPE := dbss_global.get_default_rstr_pnt (),
            p_is_clean_in                  IN     BOOLEAN := FALSE,
            p_scn_in                       IN     NUMBER := 0,
            p_is_cursor_in                 IN     BOOLEAN := dbss_global.get_default_is_cursor (),
            p_is_execute_in                IN     BOOLEAN := dbss_global.get_default_is_execute (),
            p_is_show_in                   IN     BOOLEAN := dbss_global.get_default_is_show ());
    $END

    $IF DBMS_DB_VERSION.version = 12 AND DBMS_DB_VERSION.release = 1
    $THEN
        --<> system_privilege = flashback any table
        PROCEDURE cre_rstr_pnt_pres (
            p_sqls_cv_in_out               IN OUT dbss_type.sqls_cur,
            p_rstr_pnt_in                  IN     v$restore_point.name%TYPE := dbss_global.get_default_rstr_pnt (),
            p_scn_in                       IN     NUMBER := 0,
            p_is_cursor_in                 IN     BOOLEAN := dbss_global.get_default_is_cursor (),
            p_is_execute_in                IN     BOOLEAN := dbss_global.get_default_is_execute (),
            p_is_show_in                   IN     BOOLEAN := dbss_global.get_default_is_show ());
    $END

    $IF DBMS_DB_VERSION.version > 12 OR DBMS_DB_VERSION.version = 12 AND DBMS_DB_VERSION.release > 1
    $THEN
        --<> system_privilege = flashback any table
        PROCEDURE cre_rstr_pnt_pres (
            p_sqls_cv_in_out               IN OUT dbss_type.sqls_cur,
            p_rstr_pnt_in                  IN     v$restore_point.name%TYPE := dbss_global.get_default_rstr_pnt (),
            p_is_clean_in                  IN     BOOLEAN := FALSE,
            p_scn_in                       IN     NUMBER := 0,
            p_is_cursor_in                 IN     BOOLEAN := dbss_global.get_default_is_cursor (),
            p_is_execute_in                IN     BOOLEAN := dbss_global.get_default_is_execute (),
            p_is_show_in                   IN     BOOLEAN := dbss_global.get_default_is_show ());
    $END

    --<> system_privilege = sysdba
    PROCEDURE del_rstr_pnt (
        p_sqls_cv_in_out               IN OUT dbss_type.sqls_cur,
        p_rstr_pnt_in                  IN     v$restore_point.name%TYPE := dbss_global.get_default_rstr_pnt (),
        p_is_cursor_in                 IN     BOOLEAN := dbss_global.get_default_is_cursor (),
        p_is_execute_in                IN     BOOLEAN := dbss_global.get_default_is_execute (),
        p_is_show_in                   IN     BOOLEAN := dbss_global.get_default_is_show ());

    --<> system_privilege = sysdba
    PROCEDURE del_rstr_pnts (
        p_sqls_cv_in_out               IN OUT dbss_type.sqls_cur,
        p_is_cursor_in                 IN     BOOLEAN := dbss_global.get_default_is_cursor (),
        p_is_execute_in                IN     BOOLEAN := dbss_global.get_default_is_execute (),
        p_is_show_in                   IN     BOOLEAN := dbss_global.get_default_is_show ());

    --<> system_privilege = sysdba
    PROCEDURE del_rstr_pnts_guar (
        p_sqls_cv_in_out               IN OUT dbss_type.sqls_cur,
        p_is_cursor_in                 IN     BOOLEAN := dbss_global.get_default_is_cursor (),
        p_is_execute_in                IN     BOOLEAN := dbss_global.get_default_is_execute (),
        p_is_show_in                   IN     BOOLEAN := dbss_global.get_default_is_show ());

    --<> system_privilege = flashback any table
    PROCEDURE del_rstr_pnts_norm (
        p_sqls_cv_in_out               IN OUT dbss_type.sqls_cur,
        p_is_cursor_in                 IN     BOOLEAN := dbss_global.get_default_is_cursor (),
        p_is_execute_in                IN     BOOLEAN := dbss_global.get_default_is_execute (),
        p_is_show_in                   IN     BOOLEAN := dbss_global.get_default_is_show ());

    --<> system_privilege = flashback any table
    PROCEDURE del_rstr_pnts_pres (
        p_sqls_cv_in_out               IN OUT dbss_type.sqls_cur,
        p_is_cursor_in                 IN     BOOLEAN := dbss_global.get_default_is_cursor (),
        p_is_execute_in                IN     BOOLEAN := dbss_global.get_default_is_execute (),
        p_is_show_in                   IN     BOOLEAN := dbss_global.get_default_is_show ());

    $IF DBMS_DB_VERSION.version = 12 AND DBMS_DB_VERSION.release = 1
    $THEN
        --<> object_privilege select = v$restore_point
        PROCEDURE get_rstr_pnts (
            p_rstr_pnts_cv_out                OUT dbss_type.rstr_pnts_cur,
            p_guaranteed_in                IN     v$restore_point.guarantee_flashback_database%TYPE := 'ALL',
            p_preserved_in                 IN     v$restore_point.preserved%TYPE := 'ALL',
            p_is_cursor_in                 IN     BOOLEAN := dbss_global.get_default_is_cursor (),
            p_is_show_in                   IN     BOOLEAN := dbss_global.get_default_is_show ());
    $END

    $IF DBMS_DB_VERSION.version > 12 OR DBMS_DB_VERSION.version = 12 AND DBMS_DB_VERSION.release > 1
    $THEN
        --<> object_privilege select = v$restore_point
        PROCEDURE get_rstr_pnts (
            p_rstr_pnts_cv_out                OUT dbss_type.rstr_pnts_cur,
            p_clean_in                     IN     v$restore_point.clean_pdb_restore_point%TYPE := 'ALL',
            p_guaranteed_in                IN     v$restore_point.guarantee_flashback_database%TYPE := 'ALL',
            p_preserved_in                 IN     v$restore_point.preserved%TYPE := 'ALL',
            p_is_cursor_in                 IN     BOOLEAN := dbss_global.get_default_is_cursor (),
            p_is_show_in                   IN     BOOLEAN := dbss_global.get_default_is_show ());
    $END

    --<> object_privilege select = v$database
    --<> system_privilege = alter database
    --<> system_privilege = set container
    PROCEDURE set_flashback_off (
        p_sqls_cv_in_out               IN OUT dbss_type.sqls_cur,
        p_is_cursor_in                 IN     BOOLEAN := dbss_global.get_default_is_cursor (),
        p_is_execute_in                IN     BOOLEAN := dbss_global.get_default_is_execute (),
        p_is_show_in                   IN     BOOLEAN := dbss_global.get_default_is_show ());

    --<> object_privilege select = v$database
    --<> system_privilege = alter database
    --<> system_privilege = set container
    PROCEDURE set_flashback_on (
        p_sqls_cv_in_out               IN OUT dbss_type.sqls_cur,
        p_is_cursor_in                 IN     BOOLEAN := dbss_global.get_default_is_cursor (),
        p_is_execute_in                IN     BOOLEAN := dbss_global.get_default_is_execute (),
        p_is_show_in                   IN     BOOLEAN := dbss_global.get_default_is_show ());
END dbss_flashback;
/
").

-define(TEST_13_RESULT_DEFAULT, "<?xml version='1.0' encoding='UTF-8'?>
<Package xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' xsi:noNamespaceSchemaLocation='../../priv/dbss.xsd'>
    <Name>wwe.dbss_flashback</Name>
    <Procedure>
        <Name>cre_rstr_pnt</Name>
        <Condition>
            <IfEnd>DBMS_DB_VERSION.version = 12 AND DBMS_DB_VERSION.release = 1</IfEnd>
        </Condition>
        <Privilege>
            <Type>FLASHBACK ANY TABLE</Type>
        </Privilege>
        <Parameter>
            <Name>p_sqls_cv_in_out</Name>
            <Mode>IN OUT</Mode>
            <DataType>dbss_type.sqls_cur</DataType>
        </Parameter>
        <Parameter>
            <Name>p_rstr_pnt_in</Name>
            <Mode>IN</Mode>
            <DataType>v$restore_point.NAME%TYPE</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_rstr_pnt()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_scn_in</Name>
            <Mode>IN</Mode>
            <DataType>NUMBER</DataType>
            <DefaultValue>
                <Expression>0</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_cursor_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_cursor()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_execute_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_execute()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_show_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_show()</Expression>
            </DefaultValue>
        </Parameter>
    </Procedure>
    <Procedure>
        <Name>cre_rstr_pnt</Name>
        <Condition>
            <IfEnd>DBMS_DB_VERSION.version > 12 OR DBMS_DB_VERSION.version = 12 AND DBMS_DB_VERSION.release > 1</IfEnd>
        </Condition>
        <Privilege>
            <Type>FLASHBACK ANY TABLE</Type>
        </Privilege>
        <Parameter>
            <Name>p_sqls_cv_in_out</Name>
            <Mode>IN OUT</Mode>
            <DataType>dbss_type.sqls_cur</DataType>
        </Parameter>
        <Parameter>
            <Name>p_rstr_pnt_in</Name>
            <Mode>IN</Mode>
            <DataType>v$restore_point.NAME%TYPE</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_rstr_pnt()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_clean_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>FALSE</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_scn_in</Name>
            <Mode>IN</Mode>
            <DataType>NUMBER</DataType>
            <DefaultValue>
                <Expression>0</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_cursor_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_cursor()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_execute_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_execute()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_show_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_show()</Expression>
            </DefaultValue>
        </Parameter>
    </Procedure>
    <Procedure>
        <Name>cre_rstr_pnt_guar</Name>
        <Condition>
            <IfEnd>DBMS_DB_VERSION.version = 12 AND DBMS_DB_VERSION.release = 1</IfEnd>
        </Condition>
        <Privilege>
            <Type>SYSDBA</Type>
        </Privilege>
        <Parameter>
            <Name>p_sqls_cv_in_out</Name>
            <Mode>IN OUT</Mode>
            <DataType>dbss_type.sqls_cur</DataType>
        </Parameter>
        <Parameter>
            <Name>p_rstr_pnt_in</Name>
            <Mode>IN</Mode>
            <DataType>v$restore_point.NAME%TYPE</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_rstr_pnt()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_scn_in</Name>
            <Mode>IN</Mode>
            <DataType>NUMBER</DataType>
            <DefaultValue>
                <Expression>0</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_cursor_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_cursor()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_execute_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_execute()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_show_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_show()</Expression>
            </DefaultValue>
        </Parameter>
    </Procedure>
    <Procedure>
        <Name>cre_rstr_pnt_guar</Name>
        <Condition>
            <IfEnd>DBMS_DB_VERSION.version > 12 OR DBMS_DB_VERSION.version = 12 AND DBMS_DB_VERSION.release > 1</IfEnd>
        </Condition>
        <Privilege>
            <Type>SYSDBA</Type>
        </Privilege>
        <Parameter>
            <Name>p_sqls_cv_in_out</Name>
            <Mode>IN OUT</Mode>
            <DataType>dbss_type.sqls_cur</DataType>
        </Parameter>
        <Parameter>
            <Name>p_rstr_pnt_in</Name>
            <Mode>IN</Mode>
            <DataType>v$restore_point.NAME%TYPE</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_rstr_pnt()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_clean_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>FALSE</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_scn_in</Name>
            <Mode>IN</Mode>
            <DataType>NUMBER</DataType>
            <DefaultValue>
                <Expression>0</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_cursor_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_cursor()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_execute_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_execute()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_show_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_show()</Expression>
            </DefaultValue>
        </Parameter>
    </Procedure>
    <Procedure>
        <Name>cre_rstr_pnt_pres</Name>
        <Condition>
            <IfEnd>DBMS_DB_VERSION.version = 12 AND DBMS_DB_VERSION.release = 1</IfEnd>
        </Condition>
        <Privilege>
            <Type>FLASHBACK ANY TABLE</Type>
        </Privilege>
        <Parameter>
            <Name>p_sqls_cv_in_out</Name>
            <Mode>IN OUT</Mode>
            <DataType>dbss_type.sqls_cur</DataType>
        </Parameter>
        <Parameter>
            <Name>p_rstr_pnt_in</Name>
            <Mode>IN</Mode>
            <DataType>v$restore_point.NAME%TYPE</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_rstr_pnt()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_scn_in</Name>
            <Mode>IN</Mode>
            <DataType>NUMBER</DataType>
            <DefaultValue>
                <Expression>0</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_cursor_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_cursor()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_execute_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_execute()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_show_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_show()</Expression>
            </DefaultValue>
        </Parameter>
    </Procedure>
    <Procedure>
        <Name>cre_rstr_pnt_pres</Name>
        <Condition>
            <IfEnd>DBMS_DB_VERSION.version > 12 OR DBMS_DB_VERSION.version = 12 AND DBMS_DB_VERSION.release > 1</IfEnd>
        </Condition>
        <Privilege>
            <Type>FLASHBACK ANY TABLE</Type>
        </Privilege>
        <Parameter>
            <Name>p_sqls_cv_in_out</Name>
            <Mode>IN OUT</Mode>
            <DataType>dbss_type.sqls_cur</DataType>
        </Parameter>
        <Parameter>
            <Name>p_rstr_pnt_in</Name>
            <Mode>IN</Mode>
            <DataType>v$restore_point.NAME%TYPE</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_rstr_pnt()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_clean_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>FALSE</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_scn_in</Name>
            <Mode>IN</Mode>
            <DataType>NUMBER</DataType>
            <DefaultValue>
                <Expression>0</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_cursor_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_cursor()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_execute_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_execute()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_show_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_show()</Expression>
            </DefaultValue>
        </Parameter>
    </Procedure>
    <Procedure>
        <Name>del_rstr_pnt</Name>
        <Privilege>
            <Type>SYSDBA</Type>
        </Privilege>
        <Parameter>
            <Name>p_sqls_cv_in_out</Name>
            <Mode>IN OUT</Mode>
            <DataType>dbss_type.sqls_cur</DataType>
        </Parameter>
        <Parameter>
            <Name>p_rstr_pnt_in</Name>
            <Mode>IN</Mode>
            <DataType>v$restore_point.NAME%TYPE</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_rstr_pnt()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_cursor_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_cursor()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_execute_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_execute()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_show_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_show()</Expression>
            </DefaultValue>
        </Parameter>
    </Procedure>
    <Procedure>
        <Name>del_rstr_pnts</Name>
        <Privilege>
            <Type>SYSDBA</Type>
        </Privilege>
        <Parameter>
            <Name>p_sqls_cv_in_out</Name>
            <Mode>IN OUT</Mode>
            <DataType>dbss_type.sqls_cur</DataType>
        </Parameter>
        <Parameter>
            <Name>p_is_cursor_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_cursor()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_execute_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_execute()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_show_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_show()</Expression>
            </DefaultValue>
        </Parameter>
    </Procedure>
    <Procedure>
        <Name>del_rstr_pnts_guar</Name>
        <Privilege>
            <Type>SYSDBA</Type>
        </Privilege>
        <Parameter>
            <Name>p_sqls_cv_in_out</Name>
            <Mode>IN OUT</Mode>
            <DataType>dbss_type.sqls_cur</DataType>
        </Parameter>
        <Parameter>
            <Name>p_is_cursor_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_cursor()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_execute_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_execute()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_show_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_show()</Expression>
            </DefaultValue>
        </Parameter>
    </Procedure>
    <Procedure>
        <Name>del_rstr_pnts_norm</Name>
        <Privilege>
            <Type>FLASHBACK ANY TABLE</Type>
        </Privilege>
        <Parameter>
            <Name>p_sqls_cv_in_out</Name>
            <Mode>IN OUT</Mode>
            <DataType>dbss_type.sqls_cur</DataType>
        </Parameter>
        <Parameter>
            <Name>p_is_cursor_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_cursor()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_execute_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_execute()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_show_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_show()</Expression>
            </DefaultValue>
        </Parameter>
    </Procedure>
    <Procedure>
        <Name>del_rstr_pnts_pres</Name>
        <Privilege>
            <Type>FLASHBACK ANY TABLE</Type>
        </Privilege>
        <Parameter>
            <Name>p_sqls_cv_in_out</Name>
            <Mode>IN OUT</Mode>
            <DataType>dbss_type.sqls_cur</DataType>
        </Parameter>
        <Parameter>
            <Name>p_is_cursor_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_cursor()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_execute_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_execute()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_show_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_show()</Expression>
            </DefaultValue>
        </Parameter>
    </Procedure>
    <Procedure>
        <Name>get_rstr_pnts</Name>
        <Condition>
            <IfEnd>DBMS_DB_VERSION.version = 12 AND DBMS_DB_VERSION.release = 1</IfEnd>
        </Condition>
        <Privilege>
            <Type>SELECT</Type>
            <Object>v$restore_point</Object>
        </Privilege>
        <Parameter>
            <Name>p_rstr_pnts_cv_out</Name>
            <Mode>OUT</Mode>
            <DataType>dbss_type.rstr_pnts_cur</DataType>
        </Parameter>
        <Parameter>
            <Name>p_guaranteed_in</Name>
            <Mode>IN</Mode>
            <DataType>v$restore_point.guarantee_flashback_database%TYPE</DataType>
            <DefaultValue>
                <Expression>'ALL'</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_preserved_in</Name>
            <Mode>IN</Mode>
            <DataType>v$restore_point.preserved%TYPE</DataType>
            <DefaultValue>
                <Expression>'ALL'</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_cursor_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_cursor()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_show_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_show()</Expression>
            </DefaultValue>
        </Parameter>
    </Procedure>
    <Procedure>
        <Name>get_rstr_pnts</Name>
        <Condition>
            <IfEnd>DBMS_DB_VERSION.version > 12 OR DBMS_DB_VERSION.version = 12 AND DBMS_DB_VERSION.release > 1</IfEnd>
        </Condition>
        <Privilege>
            <Type>SELECT</Type>
            <Object>v$restore_point</Object>
        </Privilege>
        <Parameter>
            <Name>p_rstr_pnts_cv_out</Name>
            <Mode>OUT</Mode>
            <DataType>dbss_type.rstr_pnts_cur</DataType>
        </Parameter>
        <Parameter>
            <Name>p_clean_in</Name>
            <Mode>IN</Mode>
            <DataType>v$restore_point.clean_pdb_restore_point%TYPE</DataType>
            <DefaultValue>
                <Expression>'ALL'</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_guaranteed_in</Name>
            <Mode>IN</Mode>
            <DataType>v$restore_point.guarantee_flashback_database%TYPE</DataType>
            <DefaultValue>
                <Expression>'ALL'</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_preserved_in</Name>
            <Mode>IN</Mode>
            <DataType>v$restore_point.preserved%TYPE</DataType>
            <DefaultValue>
                <Expression>'ALL'</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_cursor_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_cursor()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_show_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_show()</Expression>
            </DefaultValue>
        </Parameter>
    </Procedure>
    <Procedure>
        <Name>set_flashback_off</Name>
        <Privilege>
            <Type>SELECT</Type>
            <Object>v$database</Object>
        </Privilege>
        <Privilege>
            <Type>ALTER DATABASE</Type>
        </Privilege>
        <Privilege>
            <Type>SET CONTAINER</Type>
        </Privilege>
        <Parameter>
            <Name>p_sqls_cv_in_out</Name>
            <Mode>IN OUT</Mode>
            <DataType>dbss_type.sqls_cur</DataType>
        </Parameter>
        <Parameter>
            <Name>p_is_cursor_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_cursor()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_execute_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_execute()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_show_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_show()</Expression>
            </DefaultValue>
        </Parameter>
    </Procedure>
    <Procedure>
        <Name>set_flashback_on</Name>
        <Privilege>
            <Type>SELECT</Type>
            <Object>v$database</Object>
        </Privilege>
        <Privilege>
            <Type>ALTER DATABASE</Type>
        </Privilege>
        <Privilege>
            <Type>SET CONTAINER</Type>
        </Privilege>
        <Parameter>
            <Name>p_sqls_cv_in_out</Name>
            <Mode>IN OUT</Mode>
            <DataType>dbss_type.sqls_cur</DataType>
        </Parameter>
        <Parameter>
            <Name>p_is_cursor_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_cursor()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_execute_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_execute()</Expression>
            </DefaultValue>
        </Parameter>
        <Parameter>
            <Name>p_is_show_in</Name>
            <Mode>IN</Mode>
            <DataType>BOOLEAN</DataType>
            <DefaultValue>
                <Expression>dbss_global.get_default_is_show()</Expression>
            </DefaultValue>
        </Parameter>
    </Procedure>
</Package>").

-endif.