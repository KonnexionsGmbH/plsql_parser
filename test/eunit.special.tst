%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options
[{tests, []}].

%% =============================================================================
%% TESTS: Start special tests.
%% -----------------------------------------------------------------------------

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Simple tests.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
Create
Package
package_name
As
Function function_name_1
         Return Clob
         ;
End
;
".

"
Create
Package
package_name
As
Function function_name_1
         Return Clob
         ;
End
;
".

"
Create
Package
package_name
As
Function function_name_1 (parameter_name_02 Binary_double := expression_name_1)
         Return Clob
         ;
End
;
".

"
Create
Package
package_name
As
Function function_name_1 (parameter_name_02 Binary_double := expression_name_1 =  expression_name_2)
         Return Clob
         ;
End
;
".

"
Create
Package
package_name
As
Function function_name_1 (parameter_name_02 Binary_double := NOT expression_name_1)
         Return Clob
         ;
End
;
".

"
Create
Package
package_name
As
Function function_name_1 (parameter_name_02 Binary_double := NOT(expression_name_1))
         Return Clob
         ;
End
;
".

"
Create
Package
package_name
As
Function function_name_1 (parameter_name_02 Binary_double := function_name_1(NOT(expression_name_1)))
         Return Clob
         ;
End
;
".

"
Create
Package
package_name
As
Function function_name_1 (parameter_name_02 Binary_double := function_name_1(argument_name => NOT(expression_name_1)))
         Return Clob
         ;
End
;
".

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %ROWTYPE and %TYPE comment test.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
Create
Package
package_name
As
Function function_name_1 (parameter_name_01 my_rowtype%rowtype,
                          parameter_name_02 my_type_1%type,
                          parameter_name_03 my_schema.my_type_2%type)
         Return Clob
         ;
End
;
".

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Block comment test.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
Create
/*
Test
*/
Package
package_name
As
Function function_name_1
         Return Clob
         ;
End
;
".

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Annotation test.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
Create
Package
package_name
As
Function function_name_1
         Return Clob
         ;
Procedure procedure_name_1 (
               parameter_1 in out date,
               parameter_2 in out date
          )
         ;
End
;
".

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Conditional compilation test.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
Create
Package
package_name
As
$IF case_1
$THEN
Function function_name_1 (parameter_name_01 date)
         Return Clob
         ;
$ELSIF case_2
$THEN
Function function_name_2 (parameter_name_02 date)
         Return Clob
         ;
$ELSE
Function function_name_3 (parameter_name_03 date)
         Return Clob
         ;
$END
End
;
".

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Complete test.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
Create
Or Replace
Editionable
Package
package_schema.package_name
Sharing = Metadata
Accessible By (accessor_name_11,
               accessor_schema_12.accessor_name_12,
               Package accessor_name_13)
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
         Accessible By (accessor_name_21,
                        accessor_schema_22.accessor_name_22,
                        Package accessor_name_23)
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
          Accessible By (accessor_name_31,
                         accessor_schema_32.accessor_name_32,
                         Package accessor_name_33)
          ;
End package_name
;
/
".

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Real world example.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
CREATE OR REPLACE PACKAGE dbss_flashback
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

    $IF    DBMS_DB_VERSION.version > 12
        OR DBMS_DB_VERSION.version = 12 AND DBMS_DB_VERSION.release > 1
    $THEN
    PROCEDURE cre_restore_point (
        p_sqls_cv_in_out               IN OUT dbss_type.sqls_cur,
        p_restore_point_in             IN     v$restore_point.name%TYPE := dbss_global.get_default_restore_point (),
        p_is_clean_in                  IN     BOOLEAN := FALSE,
        p_scn_in                       IN     NUMBER := 0,
        p_is_cursor_in                 IN     BOOLEAN := dbss_global.get_default_is_cursor (),
        p_is_execute_in                IN     BOOLEAN := dbss_global.get_default_is_execute (),
        p_is_show_in                   IN     BOOLEAN := dbss_global.get_default_is_show ());
    $ELSE
    PROCEDURE cre_restore_point (
        p_sqls_cv_in_out               IN OUT dbss_type.sqls_cur,
        p_restore_point_in             IN     v$restore_point.name%TYPE := dbss_global.get_default_restore_point (),
        p_scn_in                       IN     NUMBER := 0,
        p_is_cursor_in                 IN     BOOLEAN := dbss_global.get_default_is_cursor (),
        p_is_execute_in                IN     BOOLEAN := dbss_global.get_default_is_execute (),
        p_is_show_in                   IN     BOOLEAN := dbss_global.get_default_is_show ());
    $END

    $IF    DBMS_DB_VERSION.version > 12
        OR DBMS_DB_VERSION.version = 12 AND DBMS_DB_VERSION.release > 1
    $THEN
    PROCEDURE cre_restore_point_guar (
        p_sqls_cv_in_out               IN OUT dbss_type.sqls_cur,
        p_restore_point_in             IN     v$restore_point.name%TYPE := dbss_global.get_default_restore_point (),
        p_is_clean_in                  IN     BOOLEAN := FALSE,
        p_scn_in                       IN     NUMBER := 0,
        p_is_cursor_in                 IN     BOOLEAN := dbss_global.get_default_is_cursor (),
        p_is_execute_in                IN     BOOLEAN := dbss_global.get_default_is_execute (),
        p_is_show_in                   IN     BOOLEAN := dbss_global.get_default_is_show ());
    $ELSE
    PROCEDURE cre_restore_point_guar (
        p_sqls_cv_in_out               IN OUT dbss_type.sqls_cur,
        p_restore_point_in             IN     v$restore_point.name%TYPE := dbss_global.get_default_restore_point (),
        p_scn_in                       IN     NUMBER := 0,
        p_is_cursor_in                 IN     BOOLEAN := dbss_global.get_default_is_cursor (),
        p_is_execute_in                IN     BOOLEAN := dbss_global.get_default_is_execute (),
        p_is_show_in                   IN     BOOLEAN := dbss_global.get_default_is_show ());
    $END

    $IF    DBMS_DB_VERSION.version > 12
        OR DBMS_DB_VERSION.version = 12 AND DBMS_DB_VERSION.release > 1
    $THEN
    PROCEDURE cre_restore_point_pres (
        p_sqls_cv_in_out               IN OUT dbss_type.sqls_cur,
        p_restore_point_in             IN     v$restore_point.name%TYPE := dbss_global.get_default_restore_point (),
        p_is_clean_in                  IN     BOOLEAN := FALSE,
        p_scn_in                       IN     NUMBER := 0,
        p_is_cursor_in                 IN     BOOLEAN := dbss_global.get_default_is_cursor (),
        p_is_execute_in                IN     BOOLEAN := dbss_global.get_default_is_execute (),
        p_is_show_in                   IN     BOOLEAN := dbss_global.get_default_is_show ());
    $ELSE
    PROCEDURE cre_restore_point_pres (
        p_sqls_cv_in_out               IN OUT dbss_type.sqls_cur,
        p_restore_point_in             IN     v$restore_point.name%TYPE := dbss_global.get_default_restore_point (),
        p_scn_in                       IN     NUMBER := 0,
        p_is_cursor_in                 IN     BOOLEAN := dbss_global.get_default_is_cursor (),
        p_is_execute_in                IN     BOOLEAN := dbss_global.get_default_is_execute (),
        p_is_show_in                   IN     BOOLEAN := dbss_global.get_default_is_show ());
    $END

    PROCEDURE del_restore_point (
        p_sqls_cv_in_out               IN OUT dbss_type.sqls_cur,
        p_restore_point_in             IN     v$restore_point.name%TYPE := dbss_global.get_default_restore_point (),
        p_is_cursor_in                 IN     BOOLEAN := dbss_global.get_default_is_cursor (),
        p_is_execute_in                IN     BOOLEAN := dbss_global.get_default_is_execute (),
        p_is_show_in                   IN     BOOLEAN := dbss_global.get_default_is_show ());

    PROCEDURE del_restore_points (
        p_sqls_cv_in_out               IN OUT dbss_type.sqls_cur,
        p_is_cursor_in                 IN     BOOLEAN := dbss_global.get_default_is_cursor (),
        p_is_execute_in                IN     BOOLEAN := dbss_global.get_default_is_execute (),
        p_is_show_in                   IN     BOOLEAN := dbss_global.get_default_is_show ());

    PROCEDURE del_restore_points_guar (
        p_sqls_cv_in_out               IN OUT dbss_type.sqls_cur,
        p_is_cursor_in                 IN     BOOLEAN := dbss_global.get_default_is_cursor (),
        p_is_execute_in                IN     BOOLEAN := dbss_global.get_default_is_execute (),
        p_is_show_in                   IN     BOOLEAN := dbss_global.get_default_is_show ());

    PROCEDURE del_restore_points_norm (
        p_sqls_cv_in_out               IN OUT dbss_type.sqls_cur,
        p_is_cursor_in                 IN     BOOLEAN := dbss_global.get_default_is_cursor (),
        p_is_execute_in                IN     BOOLEAN := dbss_global.get_default_is_execute (),
        p_is_show_in                   IN     BOOLEAN := dbss_global.get_default_is_show ());

    PROCEDURE del_restore_points_pres (
        p_sqls_cv_in_out               IN OUT dbss_type.sqls_cur,
        p_is_cursor_in                 IN     BOOLEAN := dbss_global.get_default_is_cursor (),
        p_is_execute_in                IN     BOOLEAN := dbss_global.get_default_is_execute (),
        p_is_show_in                   IN     BOOLEAN := dbss_global.get_default_is_show ());

    $IF    DBMS_DB_VERSION.version > 12
        OR DBMS_DB_VERSION.version = 12 AND DBMS_DB_VERSION.release > 1
    $THEN
    PROCEDURE get_restore_points (
        p_restore_points_cv_out           OUT dbss_type.restore_points_cur,
        p_clean_in                     IN     v$restore_point.clean_pdb_restore_point%TYPE := 'ALL',
        p_guaranteed_in                IN     v$restore_point.guarantee_flashback_database%TYPE := 'ALL',
        p_preserved_in                 IN     v$restore_point.preserved%TYPE := 'ALL',
        p_is_cursor_in                 IN     BOOLEAN := dbss_global.get_default_is_cursor (),
        p_is_show_in                   IN     BOOLEAN := dbss_global.get_default_is_show ());
    $ELSE
    PROCEDURE get_restore_points (
        p_restore_points_cv_out           OUT dbss_type.restore_points_cur,
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
".

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Privileges.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
Create
Package
package_name
As
--<> system_privilege = sysdba
--<> object_privilege execute = my_function
--<> object_privilege select = my_table
--<> system_privilege = create table
--<> object_privilege execute = my_procedure
--<> object_privilege inherit privileges = my_table
--<> object_privilege inherit remote privileges = my_table
--<> system_privilege = create any directory
Function function_name_1
         Return Clob
         ;
End
;
".

"
Create
Package
package_name
As
--<> system_privilege = sysdba
--<> object_privilege execute = my_function
--<> object_privilege select = my_table
--<> system_privilege = create table
--<> object_privilege execute = my_procedure
--<> object_privilege inherit privileges = my_table
--<> object_privilege inherit remote privileges = my_table
--<> system_privilege = create any directory
Function function_name_1
         Return Clob
         ;
--<> system_privilege = sysdba
--<> object_privilege execute = my_function
--<> object_privilege select = my_table
--<> system_privilege = create table
--<> object_privilege execute = my_procedure
--<> object_privilege inherit privileges = my_table
--<> object_privilege inherit remote privileges = my_table
--<> system_privilege = create any directory
Procedure procedure_name_1
          ;
End
;
".

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Comments.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

"
CREATE OR REPLACE PACKAGE package_1
IS
    /*
       comment
    */
    FUNCTION function_1
        RETURN DATE;

--    FUNCTION function_2 (param_1 DATE)
--        RETURN BOOLEAN;
--
--    FUNCTION function_3 (
--        param_1                               DATE,
--        param_2                        IN     DATE,
--        param_3                        IN OUT DATE,
--        param_4                           OUT DATE)
--        RETURN BOOLEAN;
END package_1;
/
".

%% -----------------------------------------------------------------------------
%% TESTS: End  special tests.
%% =============================================================================
