%% -----------------------------------------------------------------------------
%%
%% plsql_parser.yrl: PL/SQL - parser definition.
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
Header "%% Copyright (C) K2 Informatics GmbH"
"%% @private"
"%% @Author Walter Weinmann"
"%% @Email walter.weinmann@k2informatics.ch".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Nonterminals
 accessibleByClause
 accessor
 accessorCommaList
 columnRef
 columnRefCommaList
 createPackage
 dataSource
 dataSourceCommaList
 dataType
 default
 defaultCollationClause
 expression
 functionAnnotation
 functionArg
 functionArgCommaList
 functionHeading
 functionLegacyAnnotation
 functionRef
 invokerRightsClause
 literal
 objectPrivilegeAnnotation
 objectPrivilegeType
 packageFunctionDeclaration
 packageFunctionDeclarationAttribute
 packageFunctionDeclarationAttributeList
 packageItem
 packageItemConditional
 packageItemList
 packageItemSimple
 packageProcedureDeclaration
 parallelEnabledClause
 parameterAnnotation
 parameterDeclaration
 parameterDeclarationCommaList
 parameterDeclarationHelper
 parameterRef
 pipelinedClause
 plsqlPackageSource
 plsqlPackageSourceAttribute
 plsqlPackageSourceAttributeList
 plsqlScript
 plsqlUnit
 privilegeAnnotationList
 procedureAnnotation
 procedureHeading
 procedureLegacyAnnotation
 resultCacheClause
 sharingClause
 streamingClause
 systemPrivilegeAnnotation
 systemPrivilegeType
 unaryAddOrSubtract
 unitKind

 opt_or_replace
 opt_editionable
 opt_slash
 functionArgs
 endOptName
 pkgSrcTail
 pkgSrcOptShareAttr
 name
 optParams
 binary_expression
 binary_operation
.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% symbolic tokens
%% literal keyword tokens
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Terminals
 '$ELSE'
 '$ELSEIF'
 '$END'
 '$IF'
 '$THEN'
 '%ROWTYPE'
 '%TYPE'
 '('
 ')'
 '*'
 '+'
 ','
 '-'
 '--<>'
 '.'
 '/'
 ':='
 ';'
 '='
 '=>'
 '||'
 ACCESSIBLE
 ALTER
 AND
 ANY
 APPROXNUM
 ARCHIVE
 AS
 AUTHID
 BFILE
 BINARY_DOUBLE
 BINARY_FLOAT
 BINARY_INTEGER
 BLOB
 BOOLEAN
 BY
 BYTE
 CHAR
 CLOB
 CLUSTER
 COLLATION
 COMMIT
 COMPARISON
 CONTAINER
 CONTEXT
 CREATE
 CREDENTIAL
 CURRENT_USER
 CURSOR
 DATABASE
 DATE
 DAY
 DEBUG
 DEFAULT
 DEFINER
 DELETE
 DETERMINISTIC
 DIRECTORY
 DROP
 EDITIONABLE
 END
 EXECUTE
 EXTERNAL
 FALSE
 FLASHBACK
 FLOAT
 FUNCTION
 HASH
 IN
 INDEX
 INDICATOR
 INHERIT
 INSERT
 INTERVAL
 INTNUM
 IS
 JOB
 KEEP
 LEGACY_NAME_FUNCTION
 LEGACY_NAME_PROCEDURE
 LOCAL
 LOGGER_TO_CHARACTER
 LONG
 MERGE
 METADATA
 MONTH
 NAME
 NCHAR
 NCLOB
 NOCOPY
 NONE
 NONEDITIONABLE
 NOT
 NULLX
 NUMBER
 NVARCHAR2
 OBJECT_PRIVILEGE
 ON
 OR
 ORDER
 OUT
 PACKAGE
 PARALLEL_ENABLED
 PARAMETER
 PARTITION
 PIPELINED
 PLS_INTEGER
 POLYMORPHIC
 PRIVILEGES
 PROCEDURE
 QUERY
 RANGE
 RAW
 READ
 REF
 REFERENCES
 REFRESH
 RELIES_ON
 REMOTE
 REPLACE
 RESULT_CACHE
 RETURN
 REWRITE
 ROW
 ROWID
 SECOND
 SELECT
 SEQUENCE
 SESSION
 SET
 SHARING
 SQL
 STRING
 SYSDBA
 SYSTEM_PRIVILEGE
 TABLE
 TABLESPACE
 TIME
 TIMESTAMP
 TO
 TRANSLATE
 TRIGGER
 TRUE
 TYPE
 UNDER
 UNLIMITED
 UPDATE
 UROWID
 USE
 USING
 USING_NLS_COMP
 VALUE
 VARCHAR2
 VIEW
 WITH
 WRITE
 XMLTYPE
 YEAR
 ZONE
.

Rootsymbol plsqlScript.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% precedence
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Left        100 OR.
Left        200 AND.
Left        300 NOT.
Nonassoc    400 '=' COMPARISON.
Left        500 '+' '-' '||'.
Left        600 '*' '/'.
Left        700 unaryAddOrSubtract.

%% Level 01 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

plsqlScript -> plsqlUnit             : ['$1'].
plsqlScript -> plsqlUnit plsqlScript : ['$1' | '$2'].

%% Level 02 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

plsqlUnit -> createPackage : '$1'.

%% Level 03 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

createPackage ->
    CREATE opt_or_replace opt_editionable PACKAGE plsqlPackageSource ';' opt_slash
: maps_merge([#{createPackage => '$5'}, '$2', '$3', '$7']).

opt_or_replace -> '$empty'          : #{}.
opt_or_replace -> OR REPLACE        : #{orReplace => true}.

opt_editionable -> '$empty'         : #{}.
opt_editionable -> EDITIONABLE      : #{editionable => true}.
opt_editionable -> NONEDITIONABLE   : #{editionable => false}.

opt_slash -> '$empty'               : #{}.
opt_slash -> '/'                    : #{slash => true}.

%% Level 04 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

plsqlPackageSource -> name pkgSrcOptShareAttr pkgSrcTail : maps_merge([#{package => '$1'}, '$2', '$3']).

name -> NAME            :  name(['$1']).
name -> NAME '.' NAME   :  name(['$1', '$2']).

pkgSrcOptShareAttr -> '$empty'                                      : #{}.
pkgSrcOptShareAttr -> sharingClause                                 : #{sharingClause => '$1'}.
pkgSrcOptShareAttr -> plsqlPackageSourceAttributeList               : #{plsqlPackageSourceAttributeList => '$1'}.
pkgSrcOptShareAttr -> sharingClause plsqlPackageSourceAttributeList : #{sharingClause => '$1', plsqlPackageSourceAttributeList => '$2'}.

pkgSrcTail -> AS packageItemList endOptName : maps_merge([#{asIs => name(['$1']), items => '$2'}, '$3']).
pkgSrcTail -> IS packageItemList endOptName : maps_merge([#{asIs => name(['$1']), items => '$2'}, '$3']).

endOptName -> END      : #{}.
endOptName -> END NAME : #{nameend => unwrap_2_list('$2')}.

privilegeAnnotationList -> objectPrivilegeAnnotation                         : ['$1'].
privilegeAnnotationList -> objectPrivilegeAnnotation privilegeAnnotationList : ['$1' | '$2'].
privilegeAnnotationList -> systemPrivilegeAnnotation                         : ['$1'].
privilegeAnnotationList -> systemPrivilegeAnnotation privilegeAnnotationList : ['$1' | '$2'].

%% Level 05 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

objectPrivilegeAnnotation -> '--<>' OBJECT_PRIVILEGE objectPrivilegeType '=' NAME          : #{objectPrivilegeAnnotation => #{type@ => unwrap_2_list('$2'),
                                                                                                                              object@ => unwrap_2_list('$5'),
                                                                                                                              privilegeType@ => '$3'}}.
objectPrivilegeAnnotation -> '--<>' OBJECT_PRIVILEGE objectPrivilegeType '=' NAME '.' NAME : #{objectPrivilegeAnnotation => #{type@ => unwrap_2_list('$2'),
                                                                                                                              object@ => name(['$5','$7']),
                                                                                                                              privilegeType@ => '$3'}}.

packageItemList -> packageItemSimple                      : ['$1'].
packageItemList -> packageItemConditional                 : ['$1'].
packageItemList -> packageItemSimple      packageItemList : ['$1' | '$2'].
packageItemList -> packageItemConditional packageItemList : ['$1' | '$2'].

plsqlPackageSourceAttributeList -> plsqlPackageSourceAttribute                                 : ['$1'].
plsqlPackageSourceAttributeList -> plsqlPackageSourceAttribute plsqlPackageSourceAttributeList : ['$1' | '$2'].

sharingClause -> SHARING '=' METADATA : #{sharingClause => unwrap_2_list('$3')}.
sharingClause -> SHARING '=' NONE     : #{sharingClause => unwrap_2_list('$3')}.

systemPrivilegeAnnotation -> '--<>' SYSTEM_PRIVILEGE '=' systemPrivilegeType : #{systemPrivilegeAnnotation => #{type@ => unwrap_2_list('$2'),
                                                                                                                privilegeType@ => '$4'}}.

%% Level 06 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

objectPrivilegeType -> ALTER                     : unwrap_2_list('$1').
objectPrivilegeType -> DEBUG                     : unwrap_2_list('$1').
objectPrivilegeType -> DELETE                    : unwrap_2_list('$1').
objectPrivilegeType -> EXECUTE                   : unwrap_2_list('$1').
objectPrivilegeType -> FLASHBACK ARCHIVE         : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2')]).
objectPrivilegeType -> INDEX                     : unwrap_2_list('$1').
objectPrivilegeType -> INHERIT PRIVILEGES        : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2')]).
objectPrivilegeType -> INHERIT REMOTE PRIVILEGES : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2'), " ", unwrap_2_list('$3')]).
objectPrivilegeType -> INSERT                    : unwrap_2_list('$1').
objectPrivilegeType -> KEEP SEQUENCE             : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2')]).
objectPrivilegeType -> MERGE VIEW                : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2')]).
objectPrivilegeType -> ON COMMIT REFRESH         : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2'), " ", unwrap_2_list('$3')]).
objectPrivilegeType -> QUERY REWRITE             : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2')]).
objectPrivilegeType -> READ                      : unwrap_2_list('$1').
objectPrivilegeType -> REFERENCES                : unwrap_2_list('$1').
objectPrivilegeType -> SELECT                    : unwrap_2_list('$1').
objectPrivilegeType -> TRANSLATE SQL             : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2')]).
objectPrivilegeType -> UNDER                     : unwrap_2_list('$1').
objectPrivilegeType -> UPDATE                    : unwrap_2_list('$1').
objectPrivilegeType -> USE                       : unwrap_2_list('$1').
objectPrivilegeType -> WRITE                     : unwrap_2_list('$1').

packageItem -> packageFunctionDeclaration  : '$1'.
packageItem -> packageProcedureDeclaration : '$1'.

packageItemConditional -> '$ELSE'                     packageItem '$END' : #{else => '$2', 'end' => true}.
packageItemConditional -> '$ELSEIF' expression '$THEN' packageItem        : #{elseif => '$2', then => '$4'}.
packageItemConditional -> '$ELSEIF' expression '$THEN' packageItem '$END' : #{elseif => '$2', then => '$4', 'end' => true}.
packageItemConditional -> '$IF'    expression '$THEN' packageItem        : #{'if' => '$2', then => '$4'}.
packageItemConditional -> '$IF'    expression '$THEN' packageItem '$END' : #{'if' => '$2', then => '$4', 'end' => true}.

packageItemSimple -> packageItem : '$1'.

plsqlPackageSourceAttribute -> accessibleByClause     : '$1'.
plsqlPackageSourceAttribute -> defaultCollationClause : '$1'.
plsqlPackageSourceAttribute -> invokerRightsClause    : '$1'.

systemPrivilegeType -> ALTER DATABASE         : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2')]).
systemPrivilegeType -> ALTER SESSION          : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2')]).
systemPrivilegeType -> CREATE ANY CONTEXT     : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2'), " ", unwrap_2_list('$3')]).
systemPrivilegeType -> CREATE ANY CREDENTIAL  : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2'), " ", unwrap_2_list('$3')]).
systemPrivilegeType -> CREATE ANY DIRECTORY   : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2'), " ", unwrap_2_list('$3')]).
systemPrivilegeType -> CREATE CREDENTIAL      : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2')]).
systemPrivilegeType -> CREATE EXTERNAL JOB    : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2'), " ", unwrap_2_list('$3')]).
systemPrivilegeType -> CREATE JOB             : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2')]).
systemPrivilegeType -> CREATE PROCEDURE       : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2')]).
systemPrivilegeType -> CREATE SEQUENCE        : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2')]).
systemPrivilegeType -> CREATE SESSION         : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2')]).
systemPrivilegeType -> CREATE TABLE           : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2')]).
systemPrivilegeType -> CREATE TRIGGER         : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2')]).
systemPrivilegeType -> CREATE TYPE            : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2')]).
systemPrivilegeType -> CREATE VIEW            : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2')]).
systemPrivilegeType -> DROP ANY DIRECTORY     : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2'), " ", unwrap_2_list('$3')]).
systemPrivilegeType -> FLASHBACK ANY TABLE    : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2'), " ", unwrap_2_list('$3')]).
systemPrivilegeType -> INHERIT ANY PRIVILEGES : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2'), " ", unwrap_2_list('$3')]).
systemPrivilegeType -> SELECT ANY DIRECTORY   : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2'), " ", unwrap_2_list('$3')]).
systemPrivilegeType -> SELECT ANY TABLE       : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2'), " ", unwrap_2_list('$3')]).
systemPrivilegeType -> SET CONTAINER          : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2')]).
systemPrivilegeType -> SYSDBA                 : unwrap_2_list('$1').
systemPrivilegeType -> UNLIMITED TABLESPACE   : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2')]).

%% Level 07 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

accessibleByClause -> ACCESSIBLE BY accessorCommaList : #{accessibleBy => '$3'}.

defaultCollationClause -> DEFAULT COLLATION USING_NLS_COMP : #{defaultCollationClause => lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2'), " ", unwrap_2_list('$3')])}.

invokerRightsClause -> AUTHID CURRENT_USER : #{invokerRightsClause => unwrap_2_list('$2')}.
invokerRightsClause -> AUTHID DEFINER      : #{invokerRightsClause => unwrap_2_list('$2')}.

packageFunctionDeclaration ->                    functionHeading                                         ';' : '$1'.
packageFunctionDeclaration ->                    functionHeading packageFunctionDeclarationAttributeList ';' : maps_merge(['$1', #{attrs => '$2'}]).
packageFunctionDeclaration -> functionAnnotation functionHeading                                         ';' : maps_merge(['$1', '$2']).
packageFunctionDeclaration -> functionAnnotation functionHeading packageFunctionDeclarationAttributeList ';' : maps_merge(['$1', '$2', #{declAttrs => '$3'}]).

packageProcedureDeclaration ->                     procedureHeading                    ';' : '$1'.
packageProcedureDeclaration ->                     procedureHeading accessibleByClause ';' : maps_merge(['$1', '$2']).
packageProcedureDeclaration -> procedureAnnotation procedureHeading                    ';' : maps_merge(['$1', '$2']).
packageProcedureDeclaration -> procedureAnnotation procedureHeading accessibleByClause ';' : maps_merge(['$1', '$2', '$3']).

%% Level 08 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

accessorCommaList -> accessor                       : ['$1'].
accessorCommaList -> accessor ',' accessorCommaList : ['$1' | '$3'].

functionAnnotation -> functionLegacyAnnotation                         : '$1'.
functionAnnotation -> functionLegacyAnnotation privilegeAnnotationList : maps_merge(['$1', #{privilegeAnnotations => '$2'}]).
functionAnnotation -> privilegeAnnotationList                          : #{privilegeAnnotations => '$1'}.

functionHeading -> FUNCTION NAME optParams RETURN dataType : maps_merge([#{function => name(['$2']), return => '$5'}, '$3']).

packageFunctionDeclarationAttributeList -> packageFunctionDeclarationAttribute                                         : ['$1'].
packageFunctionDeclarationAttributeList -> packageFunctionDeclarationAttribute packageFunctionDeclarationAttributeList : ['$1' | '$2'].

procedureAnnotation -> functionLegacyAnnotation procedureLegacyAnnotation privilegeAnnotationList : maps_merge(['$1', '$2', #{privilegeAnnotations => '$3'}]).
procedureAnnotation -> functionLegacyAnnotation procedureLegacyAnnotation                         : maps_merge(['$1', '$2']).
procedureAnnotation -> functionLegacyAnnotation privilegeAnnotationList                           : maps_merge(['$1', #{privilegeAnnotations => '$2'}]).
procedureAnnotation -> procedureLegacyAnnotation privilegeAnnotationList                          : maps_merge(['$1', #{privilegeAnnotations => '$2'}]).
procedureAnnotation -> functionLegacyAnnotation                                                   : '$1'.
procedureAnnotation -> procedureLegacyAnnotation                                                  : '$1'.
procedureAnnotation -> privilegeAnnotationList                                                    : #{privilegeAnnotations => '$1'}.

procedureHeading -> PROCEDURE NAME optParams : maps_merge([#{procName => name(['$2'])}, '$3']).

optParams -> '$empty'                              : #{}.
optParams -> '(' parameterDeclarationCommaList ')' : #{params => '$2'}.

%% Level 09 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

accessor ->                   NAME : #{accessor => #{name@ => unwrap_2_list('$1')}}.
accessor ->          NAME '.' NAME : #{accessor => #{name@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')])}}.
accessor -> unitKind          NAME : #{accessor => #{unitKind@ => '$1',
                                                     name@ => unwrap_2_list('$2')}}.
accessor -> unitKind NAME '.' NAME : #{accessor => #{unitKind@ => '$1',
                                                     name@ => lists:append([unwrap_2_list('$2'), ".", unwrap_2_list('$4')])}}.

dataType -> BINARY_INTEGER                                                   : #{class => plsql, dataType => unwrap_2_list('$1')}.
dataType -> BOOLEAN                                                          : #{class => plsql, dataType => unwrap_2_list('$1')}.
dataType -> REF CURSOR                                                       : #{class => plsql, dataType => <<"REF CURSOR">>}.
dataType -> PLS_INTEGER                                                      : #{class => plsql, dataType => unwrap_2_list('$1')}.
dataType -> NAME                                                             : #{class => user_defined, dataType => name(['$1'])}.
dataType -> NAME          '%ROWTYPE'                                         : #{class => user_defined, dataType => name(['$1']), attribute => unwrap_2_list('$2')}.
dataType -> NAME          '%TYPE'                                            : #{class => user_defined, dataType => name(['$1']), attribute => unwrap_2_list('$2')}.
dataType -> NAME '.' NAME                                                    : #{class => user_defined, dataType => name(['$1', '$3'])}.
dataType -> NAME '.' NAME '%TYPE'                                            : #{class => user_defined, dataType => name(['$1', '$3']), attribute => unwrap_2_list('$4')}.
dataType -> BFILE                                                            : #{class => sql, dataType => unwrap_2_list('$1')}.
dataType -> BINARY_DOUBLE                                                    : #{class => sql, dataType => unwrap_2_list('$1')}.
dataType -> BINARY_FLOAT                                                     : #{class => sql, dataType => unwrap_2_list('$1')}.
dataType -> BLOB                                                             : #{class => sql, dataType => unwrap_2_list('$1')}.
dataType -> CHAR                                                             : #{class => sql, dataType => unwrap_2_list('$1')}.
dataType -> CHAR          '(' INTNUM            ')'                          : #{class => sql, dataType => unwrap_2_list('$1'), size => unwrap_2_list('$3')}.
dataType -> CHAR          '(' INTNUM     BYTE   ')'                          : #{class => sql, dataType => unwrap_2_list('$1'), size => unwrap_2_list('$3'), sizeType => unwrap_2_list('$4')}.
dataType -> CHAR          '(' INTNUM     CHAR   ')'                          : #{class => sql, dataType => unwrap_2_list('$1'), size => unwrap_2_list('$3'), sizeType => unwrap_2_list('$4')}.
dataType -> CLOB                                                             : #{class => sql, dataType => unwrap_2_list('$1')}.
dataType -> DATE                                                             : #{class => sql, dataType => unwrap_2_list('$1')}.
dataType -> FLOAT                                                            : #{class => sql, dataType => unwrap_2_list('$1')}.
dataType -> FLOAT         '(' INTNUM            ')'                          : #{class => sql, dataType => unwrap_2_list('$1'), precision =>  unwrap_2_list('$3')}.
dataType -> INTERVAL DAY                            TO SECOND                : #{class => sql, dataType => <<"INTERVAL DAY">>}.
dataType -> INTERVAL DAY  '(' INTNUM            ')' TO SECOND                : #{class => sql, dataType => <<"INTERVAL DAY">>, dayPrecision => unwrap_2_list('$4')}.
dataType -> INTERVAL DAY                            TO SECOND '(' INTNUM ')' : #{class => sql, dataType => <<"INTERVAL DAY">>, secondPrecision => unwrap_2_list('$6')}.
dataType -> INTERVAL DAY  '(' INTNUM            ')' TO SECOND '(' INTNUM ')' : #{class => sql, dataType => <<"INTERVAL DAY">>, dayPrecision => unwrap_2_list('$4'), secondPrecision => unwrap_2_list('$9')}.
dataType -> INTERVAL YEAR                           TO MONTH                 : #{class => sql, dataType => <<"INTERVAL YEAR">>}.
dataType -> INTERVAL YEAR '(' INTNUM            ')' TO MONTH                 : #{class => sql, dataType => <<"INTERVAL YEAR">>, precision => unwrap_2_list('$4')}.
dataType -> LONG RAW                                                         : #{class => sql, dataType => <<"LONG RAW">>}.
dataType -> NCHAR         '(' INTNUM            ')'                          : #{class => sql, dataType => unwrap_2_list('$1'), size => unwrap_2_list('$3')}.
dataType -> NCLOB                                                            : #{class => sql, dataType => unwrap_2_list('$1')}.
dataType -> NUMBER                                                           : #{class => sql, dataType => unwrap_2_list('$1')}.
dataType -> NUMBER        '(' INTNUM            ')'                          : #{class => sql, dataType => unwrap_2_list('$1'), precision => unwrap_2_list('$3')}.
dataType -> NUMBER        '(' INTNUM ',' INTNUM ')'                          : #{class => sql, dataType => unwrap_2_list('$1'), precision => unwrap_2_list('$3'), scale =>  unwrap_2_list('$5')}.
dataType -> NVARCHAR2     '(' INTNUM            ')'                          : #{class => sql, dataType => unwrap_2_list('$1'), size => unwrap_2_list('$3')}.
dataType -> RAW           '(' INTNUM            ')'                          : #{class => sql, dataType => unwrap_2_list('$1'), size => unwrap_2_list('$3')}.
dataType -> ROWID                                                            : #{class => sql, dataType => unwrap_2_list('$1')}.
dataType -> TIMESTAMP                                                        : #{class => sql, dataType => unwrap_2_list('$1')}.
dataType -> TIMESTAMP                               WITH       TIME ZONE     : #{class => sql, dataType => unwrap_2_list('$1'), timeZone => true}.
dataType -> TIMESTAMP                               WITH LOCAL TIME ZONE     : #{class => sql, dataType => unwrap_2_list('$1'), timeZone => true, local => true}.
dataType -> TIMESTAMP     '(' INTNUM            ')'                          : #{class => sql, dataType => unwrap_2_list('$1'), precision => unwrap_2_list('$3')}.
dataType -> TIMESTAMP     '(' INTNUM            ')' WITH       TIME ZONE     : #{class => sql, dataType => unwrap_2_list('$1'), precision => unwrap_2_list('$3'), timeZone => true}.
dataType -> TIMESTAMP     '(' INTNUM            ')' WITH LOCAL TIME ZONE     : #{class => sql, dataType => unwrap_2_list('$1'), precision => unwrap_2_list('$3'), timeZone => true, local => true}.
dataType -> UROWID                                                           : #{class => sql, dataType => unwrap_2_list('$1')}.
dataType -> UROWID        '(' INTNUM            ')'                          : #{class => sql, dataType => unwrap_2_list('$1'), size => unwrap_2_list('$3')}.
dataType -> VARCHAR2                                                         : #{class => sql, dataType => unwrap_2_list('$1')}.
dataType -> VARCHAR2      '(' INTNUM            ')'                          : #{class => sql, dataType => unwrap_2_list('$1'), size => unwrap_2_list('$3')}.
dataType -> VARCHAR2      '(' INTNUM BYTE       ')'                          : #{class => sql, dataType => unwrap_2_list('$1'), size => unwrap_2_list('$3'), sizeType => unwrap_2_list('$4')}.
dataType -> VARCHAR2      '(' INTNUM CHAR       ')'                          : #{class => sql, dataType => unwrap_2_list('$1'), size => unwrap_2_list('$3'), sizeType => unwrap_2_list('$4')}.
dataType -> XMLTYPE                                                          : #{class => sql, dataType => unwrap_2_list('$1')}.

functionLegacyAnnotation -> '--<>' LEGACY_NAME_FUNCTION '=' NAME : #{funLegacyAnnoType => unwrap_2_list('$2'), funLegacyAnnoValue => unwrap_2_list('$4')}.

packageFunctionDeclarationAttribute -> accessibleByClause    : #{packageFunctionDeclarationAttribute => '$1'}.
packageFunctionDeclarationAttribute -> DETERMINISTIC         : #{packageFunctionDeclarationAttribute => unwrap_2_list('$1')}.
packageFunctionDeclarationAttribute -> parallelEnabledClause : #{packageFunctionDeclarationAttribute => '$1'}.
packageFunctionDeclarationAttribute -> pipelinedClause       : #{packageFunctionDeclarationAttribute => '$1'}.
packageFunctionDeclarationAttribute -> resultCacheClause     : #{packageFunctionDeclarationAttribute => '$1'}.

parameterDeclarationCommaList -> parameterDeclarationHelper                                   : ['$1'].
parameterDeclarationCommaList -> parameterDeclarationHelper ',' parameterDeclarationCommaList : ['$1' | '$3'].

parameterDeclarationHelper ->                     parameterDeclaration : '$1'.
parameterDeclarationHelper -> parameterAnnotation parameterDeclaration : maps_merge(['$1', '$2']).

procedureLegacyAnnotation -> '--<>' LEGACY_NAME_PROCEDURE '=' NAME : #{procLegacyAnnoType => unwrap_2_list('$2'),
                                                                       procLegacyAnnovalue => unwrap_2_list('$4')}.

%% Level 10 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parameterAnnotation -> '--<>' LOGGER_TO_CHARACTER '=' FALSE : #{parameterAnnotation => #{type@ => unwrap_2_list('$2'),
                                                                                         value@ => unwrap_2_list('$4')}}.
parameterAnnotation -> '--<>' LOGGER_TO_CHARACTER '=' NONE : #{parameterAnnotation => #{type@ => unwrap_2_list('$2'),
                                                                                         value@ => unwrap_2_list('$4')}}.

parameterDeclaration -> NAME               dataType         : #{paramName => name(['$1']), paramType => '$2'}.
parameterDeclaration -> NAME               dataType default : maps_merge([#{paramName => name(['$1']), paramType => '$2'}, '$3']).
parameterDeclaration -> NAME IN            dataType         : #{paramName => name(['$1']), mode => unwrap_2_list('$2'), paramType => '$3'}.
parameterDeclaration -> NAME IN            dataType default : maps_merge([#{paramName => name(['$1']), mode => unwrap_2_list('$2'), paramType => '$3'}, '$4']).
parameterDeclaration -> NAME    OUT        dataType         : #{paramName => name(['$1']), mode => unwrap_2_list('$2'), paramType => '$3'}.
parameterDeclaration -> NAME    OUT NOCOPY dataType         : #{paramName => name(['$1']), mode => unwrap_2_list('$2'), nocopy => unwrap_2_list('$3'), paramType => '$4'}.
parameterDeclaration -> NAME IN OUT        dataType         : #{paramName => name(['$1']), mode => <<"IN OUT">>, paramType => '$4'}.
parameterDeclaration -> NAME IN OUT NOCOPY dataType         : #{paramName => name(['$1']), mode => <<"IN OUT">>, nocopy => unwrap_2_list('$4'), paramType => '$5'}.

unitKind -> FUNCTION  : unwrap_2_list('$1').
unitKind -> PACKAGE   : unwrap_2_list('$1').
unitKind -> PROCEDURE : unwrap_2_list('$1').
unitKind -> TRIGGER   : unwrap_2_list('$1').
unitKind -> TYPE      : unwrap_2_list('$1').

%% Level 11 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

default -> ':='    expression : #{default => #{type => unwrap_2_list('$1'), value => '$2'}}.
default -> DEFAULT expression : #{default => #{type => unwrap_2_list('$1'), value => '$2'}}.

expression -> columnRef                        : '$1'.
expression -> functionRef                      : '$1'.
expression -> literal                          : '$1'.
expression -> NULLX                            : 'NULLX'.
expression -> parameterRef                     : '$1'.
expression -> '(' expression ')'               : #{operator => '(', operand => '$2'}.
expression -> 'NOT'                 expression : #{operator => 'NOT', operand => '$2'}.
expression -> unaryAddOrSubtract    expression : #{operator => '$1', operand => '$2'}.
expression -> binary_expression                : '$1'.

binary_expression -> expression binary_operation expression : #{operator => '$2', operandLeft => '$1', operandRight => '$3'}.

binary_operation -> 'AND'       : 'AND'.
binary_operation -> 'OR'        : 'OR'.
binary_operation -> '+'         : '+'.
binary_operation -> '-'         : '-'.
binary_operation -> '/'         : '/'.
binary_operation -> '*'         : '*'.
binary_operation -> '||'        : '||'.
binary_operation -> '='         : '='.
binary_operation -> COMPARISON  : unwrap_2_atom('$1').

parallelEnabledClause -> PARALLEL_ENABLED '(' PARTITION NAME BY ANY                                              ')' : #{parallelEnabledClause => #{name@ => unwrap_2_list('$4'),
                                                                                                                                                    type@ => unwrap_2_list('$6')}}.
parallelEnabledClause -> PARALLEL_ENABLED '(' PARTITION NAME BY HASH  '(' columnRefCommaList ')'                 ')' : #{parallelEnabledClause => #{name@ => unwrap_2_list('$4'),
                                                                                                                                                    type@ => unwrap_2_list('$6'),
                                                                                                                                                    columnRefCommaList@ => '$8'}}.
parallelEnabledClause -> PARALLEL_ENABLED '(' PARTITION NAME BY HASH  '(' columnRefCommaList ')' streamingClause ')' : #{parallelEnabledClause => #{name@ => unwrap_2_list('$4'),
                                                                                                                                                    type@ => unwrap_2_list('$6'),
                                                                                                                                                    columnRefCommaList@ => '$8',
                                                                                                                                                    streamingClause@ => '$10'}}.
parallelEnabledClause -> PARALLEL_ENABLED '(' PARTITION NAME BY RANGE '(' columnRefCommaList ')'                 ')' : #{parallelEnabledClause => #{name@ => unwrap_2_list('$4'),
                                                                                                                                                    type@ => unwrap_2_list('$6'),
                                                                                                                                                    columnRefCommaList@ => '$8'}}.
parallelEnabledClause -> PARALLEL_ENABLED '(' PARTITION NAME BY RANGE '(' columnRefCommaList ')' streamingClause ')' : #{parallelEnabledClause => #{name@ => unwrap_2_list('$4'),
                                                                                                                                                    type@ => unwrap_2_list('$6'),
                                                                                                                                                    columnRefCommaList@ => '$8',
                                                                                                                                                    streamingClause@ => '$10'}}.
parallelEnabledClause -> PARALLEL_ENABLED '(' PARTITION NAME BY VALUE '(' columnRef          ')'                 ')' : #{parallelEnabledClause => #{name@ => unwrap_2_list('$4'),
                                                                                                                                                    type@ => unwrap_2_list('$6'),
                                                                                                                                                    columnRefCommaList@ => ['$8']}}.

pipelinedClause -> PIPELINED                   USING          NAME : #{pipelinedClause => #{implementationPackage@ => unwrap_2_list('$3')}}.
pipelinedClause -> PIPELINED                   USING NAME '.' NAME : #{pipelinedClause => #{implementationPackage@ => name(['$3','$5'])}}.
pipelinedClause -> PIPELINED ROW   POLYMORPHIC USING          NAME : #{pipelinedClause => #{type@ => unwrap_2_list('$2'),
                                                                                            implementationPackage@ => unwrap_2_list('$5')}}.
pipelinedClause -> PIPELINED ROW   POLYMORPHIC USING NAME '.' NAME : #{pipelinedClause => #{type@ => unwrap_2_list('$2'),
                                                                                            implementationPackage@ => name(['$5','$7'])}}.
pipelinedClause -> PIPELINED TABLE POLYMORPHIC USING          NAME : #{pipelinedClause => #{type@ => unwrap_2_list('$2'),
                                                                                            implementationPackage@ => unwrap_2_list('$5')}}.
pipelinedClause -> PIPELINED TABLE POLYMORPHIC USING NAME '.' NAME : #{pipelinedClause => #{type@ => unwrap_2_list('$2'),
                                                                                            implementationPackage@ => name(['$5', '$7'])}}.

resultCacheClause -> RESULT_CACHE                                       : #{resultCache => []}.
resultCacheClause -> RESULT_CACHE RELIES_ON '(' dataSourceCommaList ')' : #{resultCache => '$4'}.

%% Level 12 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

columnRef -> NAME                               : name(['$1']).
columnRef -> NAME '.' NAME                      : name(['$1', '$3']).
columnRef -> NAME '.' NAME '.' NAME             : name(['$1', '$3', '$5']).
columnRef -> NAME '(' '+' ')'                   : name(['$1']) ++ "(+)".
columnRef -> NAME '.' NAME '(' '+' ')'          : name(['$1', '$3']) ++ "(+)".
columnRef -> NAME '.' NAME '.' NAME '(' '+' ')' : name(['$1', '$3', '$5']) ++"(+)".
columnRef -> NAME '.' '*'                       : name(['$1', "*"]).
columnRef -> NAME '.' NAME '.' '*'              : name(['$1', '$3', ".*"]).

dataSourceCommaList -> dataSource                         : ['$1'].
dataSourceCommaList -> dataSource ',' dataSourceCommaList : ['$1' | '$3'].

streamingClause -> ORDER   expression BY '(' columnRefCommaList ')' : #{streamingClause => #{type@ => unwrap_2_list('$1'),
                                                                                             expression@ => '$2',
                                                                                             columnRefCommaList@ => '$5'}}.
streamingClause -> CLUSTER expression BY '(' columnRefCommaList ')' : #{streamingClause => #{type@ => unwrap_2_list('$1'),
                                                                                             expression@ => '$2',
                                                                                             columnRefCommaList@ => '$5'}}.

%% Level 13 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

columnRefCommaList -> columnRef                        : ['$1'].
columnRefCommaList -> columnRef ',' columnRefCommaList : ['$1' | '$3'].

dataSource ->          NAME : #{dataSource => unwrap_2_list('$1')}.
dataSource -> NAME '.' NAME : #{dataSource => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')])}.

%% Level 14 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

functionRef -> NAME functionArgs : #{functionRef => name(['$1']), args => '$2'}.
functionRef -> NAME '.' NAME functionArgs : #{functionRef => name(['$1', '$3']), args => '$4'}.
functionRef -> NAME '.' NAME '.' NAME functionArgs : #{functionRef => name(['$1', '$3', '$5']), args => '$6'}.

literal -> APPROXNUM : #{literal => unwrap_2_list('$1')}.
literal -> FALSE     : #{literal => unwrap_2_list('$1')}.
literal -> INTNUM    : #{literal => unwrap_2_list('$1')}.
literal -> STRING    : #{literal => unwrap_2_list('$1')}.
literal -> TRUE      : #{literal => unwrap_2_list('$1')}.

parameterRef -> PARAMETER                     : #{parameterRef => unwrap_2_list('$1')}.
parameterRef -> PARAMETER           PARAMETER : #{parameterRef => #{parameterLeft@ => unwrap_2_list('$1'),
                                                                    parameterRight@ => unwrap_2_list('$2')}}.
parameterRef -> PARAMETER INDICATOR PARAMETER : #{parameterRef => #{indicator@ => true,
                                                                    parameterLeft@ => unwrap_2_list('$1'),
                                                                    parameterRight@ => unwrap_2_list('$3')}}.

unaryAddOrSubtract -> '+' : unwrap_2_list('$1').
unaryAddOrSubtract -> '-' : unwrap_2_list('$1').

%% Level 15 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


functionArgs -> '('                      ')' : [].
functionArgs -> '(' functionArgCommaList ')' : '$2'.

functionArgCommaList -> functionArg                          : ['$1'].
functionArgCommaList -> functionArg ',' functionArgCommaList : ['$1' | '$3'].

%% Level 16 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

functionArg -> expression            : '$1'.
functionArg -> NAME '=>' expression  : #{name => unwrap_2_list('$1'), value => '$3'}.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Erlang code.

%% -----------------------------------------------------------------------------
%%
%% plsql_parser.erl: PL/SQL - parser.
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

% parser and compiler interface
-export([
    is_reserved/1,
    parsetree/1,
    parsetree_with_tokens/1
]).

-define(NODEBUG, true).

-include("plsql_parser.hrl").

%%------------------------------------------------------------------------------
%%                          parser helper functions
%%------------------------------------------------------------------------------

name([Name]) when is_list(Name) -> Name;
name([Name]) when is_tuple(Name) -> unwrap_2_list(Name);
name([Name|Names]) when is_tuple(Name) ->
    name([unwrap_2_list(Name)|Names]);
name([Name|Names]) when is_list(Name) ->
    Name ++ "." ++ name(Names).

maps_merge(Maps) when is_list(Maps) -> maps_merge(Maps, #{}).
maps_merge([], Map) -> Map;
maps_merge([M|Maps], Map) -> maps_merge(Maps, maps:merge(Map, M)).

unwrap_2_atom({_, _, X}) when is_atom(X) ->
    X.

unwrap_2_list({X, _}) when is_atom(X) ->
    atom_to_list(X);
unwrap_2_list({_, _, X}) when is_list(X) ->
    X;
unwrap_2_list({_, _, X}) when is_atom(X) ->
    atom_to_list(X).

%%------------------------------------------------------------------------------
%%                                  PARSER
%%------------------------------------------------------------------------------

-spec parsetree(binary()|list()) ->
    {parse_error, term()} | {lex_error, term()} | {ok, [tuple()]}.
parsetree(Source) ->
    ?D("Start~n Source: ~p~n", [Source]),
    case parsetree_with_tokens(Source) of
        {ok, {ParseTree, _Tokens}} ->
            ?D("~n ParseTree: ~p~n Tokens: ~p~n", [ParseTree, _Tokens]),
            {ok, ParseTree};
        Error -> Error
    end.

-spec parsetree_with_tokens(binary()|list()) ->
    {parse_error, term()} | {lex_error, term()} | {ok, {[tuple()], list()}}.
parsetree_with_tokens([]) -> {parse_error, invalid_string};
parsetree_with_tokens(<<>>) -> {parse_error, invalid_string};
parsetree_with_tokens(Source) ->
    ?D("Start~n Source: ~p~n", [Source]),
    [C | _] = lists:reverse(Source),
    NSource = if C =:= $; -> Source; true -> string:trim(Source) end,
    case plsql_lexer:string(NSource) of
        {ok, Toks, _} ->
            case parse(Toks) of
                {ok, PTree} ->
                    ?D("~n ParseTree: ~p~n Tokens: ~p~n", [PTree, Toks]),
                    {ok, {PTree, Toks}};
                {error, {N, ?MODULE, ErrorTerms}} ->
                    {parse_error, {lists:flatten(
                        [integer_to_list(N), ": ", ErrorTerms]), Toks}};
                {error, Error} -> {parse_error, {Error, Toks}}
            end;
        {error, Error, _} -> {lex_error, Error}
    end.

-spec is_reserved(binary() | atom() | list()) -> true | false.
is_reserved(Word) when is_binary(Word) ->
    is_reserved(erlang:binary_to_list(Word));
is_reserved(Word) when is_atom(Word) ->
    is_reserved(erlang:atom_to_list(Word));
is_reserved(Word) when is_list(Word) ->
    lists:member(erlang:list_to_atom(string:to_upper(Word)),
        plsql_lexer:reserved_keywords()).
