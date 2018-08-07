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
 privilegeRoleAnnotationList
 procedureAnnotation
 procedureHeading
 procedureLegacyAnnotation
 resultCacheClause
 roleAnnotation
 sharingClause
 streamingClause
 systemPrivilegeAnnotation
 systemPrivilegeType
 unaryAddOrSubtract
 unitKind
.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% symbolic tokens
%% literal keyword tokens
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Terminals
 '$ELSE'
 '$ELSIF'
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
 ROLE
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
plsqlScript -> plsqlUnit plsqlScript : [['$1'] | '$2'].

%% Level 02 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

plsqlUnit -> createPackage : #{plsqlUnit => #{createPackage@ => '$1'}}.

%% Level 03 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

createPackage -> CREATE                           PACKAGE plsqlPackageSource ';'     : #{createPackage => #{plsqlPackageSource@ => '$3'}}.
createPackage -> CREATE                           PACKAGE plsqlPackageSource ';' '/' : #{createPackage => #{plsqlPackageSource@ => '$3',
                                                                                                            slash@ => true}}.
createPackage -> CREATE               EDITIONABLE PACKAGE plsqlPackageSource ';'     : #{createPackage => #{editionable@ => unwrap_2_list('$2'),
                                                                                                            plsqlPackageSource@ => '$4'}}.
createPackage -> CREATE               EDITIONABLE PACKAGE plsqlPackageSource ';' '/' : #{createPackage => #{editionable@ => unwrap_2_list('$2'),
                                                                                                            plsqlPackageSource@ => '$4',
                                                                                                            slash@ => true}}.
createPackage -> CREATE            NONEDITIONABLE PACKAGE plsqlPackageSource ';'     : #{createPackage => #{editionable@ => unwrap_2_list('$2'),
                                                                                                            plsqlPackageSource@ => '$4'}}.
createPackage -> CREATE            NONEDITIONABLE PACKAGE plsqlPackageSource ';' '/' : #{createPackage => #{editionable@ => unwrap_2_list('$2'),
                                                                                                            plsqlPackageSource@ => '$4',
                                                                                                            slash@ => true}}.
createPackage -> CREATE OR REPLACE                PACKAGE plsqlPackageSource ';'     : #{createPackage => #{orReplace@ => "OR REPLACE",
                                                                                                            plsqlPackageSource@ => '$5'}}.
createPackage -> CREATE OR REPLACE                PACKAGE plsqlPackageSource ';' '/' : #{createPackage => #{orReplace@ => "OR REPLACE",
                                                                                                            plsqlPackageSource@ => '$5',
                                                                                                            slash@ => true}}.
createPackage -> CREATE OR REPLACE    EDITIONABLE PACKAGE plsqlPackageSource ';'     : #{createPackage => #{orReplace@ => "OR REPLACE",
                                                                                                            editionable@ => unwrap_2_list('$4'),
                                                                                                            plsqlPackageSource@ => '$6'}}.
createPackage -> CREATE OR REPLACE    EDITIONABLE PACKAGE plsqlPackageSource ';' '/' : #{createPackage => #{orReplace@ => "OR REPLACE",
                                                                                                            editionable@ => unwrap_2_list('$4'),
                                                                                                            plsqlPackageSource@ => '$6',
                                                                                                            slash@ => true}}.
createPackage -> CREATE OR REPLACE NONEDITIONABLE PACKAGE plsqlPackageSource ';'     : #{createPackage => #{orReplace@ => "OR REPLACE",
                                                                                                            editionable@ => unwrap_2_list('$4'),
                                                                                                            plsqlPackageSource@ => '$6'}}.
createPackage -> CREATE OR REPLACE NONEDITIONABLE PACKAGE plsqlPackageSource ';' '/' : #{createPackage => #{orReplace@ => "OR REPLACE",
                                                                                                            editionable@ => unwrap_2_list('$4'),
                                                                                                            plsqlPackageSource@ => '$6',
                                                                                                            slash@ => true}}.

%% Level 04 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

plsqlPackageSource ->          NAME                                               AS packageItemList END      : #{plsqlPackageSource => #{packageNameStart@ => unwrap_2_list('$1'),
                                                                                                                                          asIs@ => unwrap_2_list('$2'),
                                                                                                                                          packageItemList@ => '$3'}}.
plsqlPackageSource ->          NAME                                               IS packageItemList END      : #{plsqlPackageSource => #{packageNameStart@ => unwrap_2_list('$1'),
                                                                                                                                          asIs@ => unwrap_2_list('$2'),
                                                                                                                                          packageItemList@ => '$3'}}.
plsqlPackageSource ->          NAME                                               AS packageItemList END NAME : #{plsqlPackageSource => #{packageNameStart@ => unwrap_2_list('$1'),
                                                                                                                                          asIs@ => unwrap_2_list('$2'),
                                                                                                                                          packageItemList@ => '$3',
                                                                                                                                          packageNameEnd@ => unwrap_2_list('$5')}}.
plsqlPackageSource ->          NAME                                               IS packageItemList END NAME : #{plsqlPackageSource => #{packageNameStart@ => unwrap_2_list('$1'),
                                                                                                                                          asIs@ => unwrap_2_list('$2'),
                                                                                                                                          packageItemList@ => '$3',
                                                                                                                                          packageNameEnd@ => unwrap_2_list('$5')}}.
plsqlPackageSource ->          NAME               plsqlPackageSourceAttributeList AS packageItemList END      : #{plsqlPackageSource => #{packageNameStart@ => unwrap_2_list('$1'),
                                                                                                                                          plsqlPackageSourceAttributeList@ => '$2',
                                                                                                                                          asIs@ => unwrap_2_list('$3'),
                                                                                                                                          packageItemList@ => '$4'}}.
plsqlPackageSource ->          NAME               plsqlPackageSourceAttributeList IS packageItemList END      : #{plsqlPackageSource => #{packageNameStart@ => unwrap_2_list('$1'),
                                                                                                                                          plsqlPackageSourceAttributeList@ => '$2',
                                                                                                                                          asIs@ => unwrap_2_list('$3'),
                                                                                                                                          packageItemList@ => '$4'}}.
plsqlPackageSource ->          NAME               plsqlPackageSourceAttributeList AS packageItemList END NAME : #{plsqlPackageSource => #{packageNameStart@ => unwrap_2_list('$1'),
                                                                                                                                          plsqlPackageSourceAttributeList@ => '$2',
                                                                                                                                          asIs@ => unwrap_2_list('$3'),
                                                                                                                                          packageItemList@ => '$4',
                                                                                                                                          packageNameEnd@ => unwrap_2_list('$6')}}.
plsqlPackageSource ->          NAME               plsqlPackageSourceAttributeList IS packageItemList END NAME : #{plsqlPackageSource => #{packageNameStart@ => unwrap_2_list('$1'),
                                                                                                                                          plsqlPackageSourceAttributeList@ => '$2',
                                                                                                                                          asIs@ => unwrap_2_list('$3'),
                                                                                                                                          packageItemList@ => '$4',
                                                                                                                                          packageNameEnd@ => unwrap_2_list('$6')}}.
plsqlPackageSource ->          NAME sharingClause                                 AS packageItemList END      : #{plsqlPackageSource => #{packageNameStart@ => unwrap_2_list('$1'),
                                                                                                                                          sharingClause@ => '$2',
                                                                                                                                          asIs@ => unwrap_2_list('$3'),
                                                                                                                                          packageItemList@ => '$4'}}.
plsqlPackageSource ->          NAME sharingClause                                 IS packageItemList END      : #{plsqlPackageSource => #{packageNameStart@ => unwrap_2_list('$1'),
                                                                                                                                          sharingClause@ => '$2',
                                                                                                                                          asIs@ => unwrap_2_list('$3'),
                                                                                                                                          packageItemList@ => '$4'}}.
plsqlPackageSource ->          NAME sharingClause                                 AS packageItemList END NAME : #{plsqlPackageSource => #{packageNameStart@ => unwrap_2_list('$1'),
                                                                                                                                          sharingClause@ => '$2',
                                                                                                                                          asIs@ => unwrap_2_list('$3'),
                                                                                                                                          packageItemList@ => '$4',
                                                                                                                                          packageNameEnd@ => unwrap_2_list('$6')}}.
plsqlPackageSource ->          NAME sharingClause                                 IS packageItemList END NAME : #{plsqlPackageSource => #{packageNameStart@ => unwrap_2_list('$1'),
                                                                                                                                          sharingClause@ => '$2',
                                                                                                                                          asIs@ => unwrap_2_list('$3'),
                                                                                                                                          packageItemList@ => '$4',
                                                                                                                                          packageNameEnd@ => unwrap_2_list('$6')}}.
plsqlPackageSource ->          NAME sharingClause plsqlPackageSourceAttributeList AS packageItemList END      : #{plsqlPackageSource => #{packageNameStart@ => unwrap_2_list('$1'),
                                                                                                                                          sharingClause@ => '$2',
                                                                                                                                          plsqlPackageSourceAttributeList@ => '$3',
                                                                                                                                          asIs@ => unwrap_2_list('$4'),
                                                                                                                                          packageItemList@ => '$5'}}.
plsqlPackageSource ->          NAME sharingClause plsqlPackageSourceAttributeList IS packageItemList END      : #{plsqlPackageSource => #{packageNameStart@ => unwrap_2_list('$1'),
                                                                                                                                          sharingClause@ => '$2',
                                                                                                                                          plsqlPackageSourceAttributeList@ => '$3',
                                                                                                                                          asIs@ => unwrap_2_list('$4'),
                                                                                                                                          packageItemList@ => '$5'}}.
plsqlPackageSource ->          NAME sharingClause plsqlPackageSourceAttributeList AS packageItemList END NAME : #{plsqlPackageSource => #{packageNameStart@ => unwrap_2_list('$1'),
                                                                                                                                          sharingClause@ => '$2',
                                                                                                                                          plsqlPackageSourceAttributeList@ => '$3',
                                                                                                                                          asIs@ => unwrap_2_list('$4'),
                                                                                                                                          packageItemList@ => '$5',
                                                                                                                                          packageNameEnd@ => unwrap_2_list('$7')}}.
plsqlPackageSource ->          NAME sharingClause plsqlPackageSourceAttributeList IS packageItemList END NAME : #{plsqlPackageSource => #{packageNameStart@ => unwrap_2_list('$1'),
                                                                                                                                          sharingClause@ => '$2',
                                                                                                                                          plsqlPackageSourceAttributeList@ => '$3',
                                                                                                                                          asIs@ => unwrap_2_list('$4'),
                                                                                                                                          packageItemList@ => '$5',
                                                                                                                                          packageNameEnd@ => unwrap_2_list('$7')}}.
plsqlPackageSource -> NAME '.' NAME                                               AS packageItemList END      : #{plsqlPackageSource => #{packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')]),
                                                                                                                                          asIs@ => unwrap_2_list('$4'),
                                                                                                                                          packageItemList@ => '$5'}}.
plsqlPackageSource -> NAME '.' NAME                                               IS packageItemList END      : #{plsqlPackageSource => #{packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')]),
                                                                                                                                          asIs@ => unwrap_2_list('$4'),
                                                                                                                                          packageItemList@ => '$5'}}.
plsqlPackageSource -> NAME '.' NAME                                               AS packageItemList END NAME : #{plsqlPackageSource => #{packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')]),
                                                                                                                                          asIs@ => unwrap_2_list('$4'),
                                                                                                                                          packageItemList@ => '$5',
                                                                                                                                          packageNameEnd@ => unwrap_2_list('$7')}}.
plsqlPackageSource -> NAME '.' NAME                                               IS packageItemList END NAME : #{plsqlPackageSource => #{packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')]),
                                                                                                                                          asIs@ => unwrap_2_list('$4'),
                                                                                                                                          packageItemList@ => '$5',
                                                                                                                                          packageNameEnd@ => unwrap_2_list('$7')}}.
plsqlPackageSource -> NAME '.' NAME               plsqlPackageSourceAttributeList AS packageItemList END      : #{plsqlPackageSource => #{packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')]),
                                                                                                                                          plsqlPackageSourceAttributeList@ => '$4',
                                                                                                                                          asIs@ => unwrap_2_list('$5'),
                                                                                                                                          packageItemList@ => '$6'}}.
plsqlPackageSource -> NAME '.' NAME               plsqlPackageSourceAttributeList IS packageItemList END      : #{plsqlPackageSource => #{packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')]),
                                                                                                                                          plsqlPackageSourceAttributeList@ => '$4',
                                                                                                                                          asIs@ => unwrap_2_list('$5'),
                                                                                                                                          packageItemList@ => '$6'}}.
plsqlPackageSource -> NAME '.' NAME               plsqlPackageSourceAttributeList AS packageItemList END NAME : #{plsqlPackageSource => #{packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')]),
                                                                                                                                          plsqlPackageSourceAttributeList@ => '$4',
                                                                                                                                          asIs@ => unwrap_2_list('$5'),
                                                                                                                                          packageItemList@ => '$6',
                                                                                                                                          packageNameEnd@ => unwrap_2_list('$8')}}.
plsqlPackageSource -> NAME '.' NAME               plsqlPackageSourceAttributeList IS packageItemList END NAME : #{plsqlPackageSource => #{packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')]),
                                                                                                                                          plsqlPackageSourceAttributeList@ => '$4',
                                                                                                                                          asIs@ => unwrap_2_list('$5'),
                                                                                                                                          packageItemList@ => '$6',
                                                                                                                                          packageNameEnd@ => unwrap_2_list('$8')}}.
plsqlPackageSource -> NAME '.' NAME sharingClause                                 AS packageItemList END      : #{plsqlPackageSource => #{packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')]),
                                                                                                                                          sharingClause@ => '$4',
                                                                                                                                          asIs@ => unwrap_2_list('$5'),
                                                                                                                                          packageItemList@ => '$6'}}.
plsqlPackageSource -> NAME '.' NAME sharingClause                                 IS packageItemList END      : #{plsqlPackageSource => #{packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')]),
                                                                                                                                          sharingClause@ => '$4',
                                                                                                                                          asIs@ => unwrap_2_list('$5'),
                                                                                                                                          packageItemList@ => '$6'}}.
plsqlPackageSource -> NAME '.' NAME sharingClause                                 AS packageItemList END NAME : #{plsqlPackageSource => #{packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')]),
                                                                                                                                          sharingClause@ => '$4',
                                                                                                                                          asIs@ => unwrap_2_list('$5'),
                                                                                                                                          packageItemList@ => '$6',
                                                                                                                                          packageNameEnd@ => unwrap_2_list('$8')}}.
plsqlPackageSource -> NAME '.' NAME sharingClause                                 IS packageItemList END NAME : #{plsqlPackageSource => #{packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')]),
                                                                                                                                          sharingClause@ => '$4',
                                                                                                                                          asIs@ => unwrap_2_list('$5'),
                                                                                                                                          packageItemList@ => '$6',
                                                                                                                                          packageNameEnd@ => unwrap_2_list('$8')}}.
plsqlPackageSource -> NAME '.' NAME sharingClause plsqlPackageSourceAttributeList AS packageItemList END      : #{plsqlPackageSource => #{packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')]),
                                                                                                                                          sharingClause@ => '$4',
                                                                                                                                          plsqlPackageSourceAttributeList@ => '$5',
                                                                                                                                          asIs@ => unwrap_2_list('$6'),
                                                                                                                                          packageItemList@ => '$7'}}.
plsqlPackageSource -> NAME '.' NAME sharingClause plsqlPackageSourceAttributeList IS packageItemList END      : #{plsqlPackageSource => #{packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')]),
                                                                                                                                          sharingClause@ => '$4',
                                                                                                                                          plsqlPackageSourceAttributeList@ => '$5',
                                                                                                                                          asIs@ => unwrap_2_list('$6'),
                                                                                                                                          packageItemList@ => '$7'}}.
plsqlPackageSource -> NAME '.' NAME sharingClause plsqlPackageSourceAttributeList AS packageItemList END NAME : #{plsqlPackageSource => #{packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')]),
                                                                                                                                          sharingClause@ => '$4',
                                                                                                                                          plsqlPackageSourceAttributeList@ => '$5',
                                                                                                                                          asIs@ => unwrap_2_list('$6'),
                                                                                                                                          packageItemList@ => '$7',
                                                                                                                                          packageNameEnd@ => unwrap_2_list('$9')}}.
plsqlPackageSource -> NAME '.' NAME sharingClause plsqlPackageSourceAttributeList IS packageItemList END NAME : #{plsqlPackageSource => #{packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')]),
                                                                                                                                          sharingClause@ => '$4',
                                                                                                                                          plsqlPackageSourceAttributeList@ => '$5',
                                                                                                                                          asIs@ => unwrap_2_list('$6'),
                                                                                                                                          packageItemList@ => '$7',
                                                                                                                                          packageNameEnd@ => unwrap_2_list('$9')}}.

privilegeRoleAnnotationList -> objectPrivilegeAnnotation                             : ['$1'].
privilegeRoleAnnotationList -> objectPrivilegeAnnotation privilegeRoleAnnotationList : ['$1' | '$2'].
privilegeRoleAnnotationList -> roleAnnotation                                        : ['$1'].
privilegeRoleAnnotationList -> roleAnnotation            privilegeRoleAnnotationList : ['$1' | '$2'].
privilegeRoleAnnotationList -> systemPrivilegeAnnotation                             : ['$1'].
privilegeRoleAnnotationList -> systemPrivilegeAnnotation privilegeRoleAnnotationList : ['$1' | '$2'].

%% Level 05 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

objectPrivilegeAnnotation -> '--<>' OBJECT_PRIVILEGE objectPrivilegeType '=' NAME          : #{objectPrivilegeAnnotation => #{type@ => unwrap_2_list('$2'),
                                                                                                                              object@ => unwrap_2_list('$5'),
                                                                                                                              privilegeType@ => '$3'}}.
objectPrivilegeAnnotation -> '--<>' OBJECT_PRIVILEGE objectPrivilegeType '=' NAME '.' NAME : #{objectPrivilegeAnnotation => #{type@ => unwrap_2_list('$2'),
                                                                                                                              object@ => lists:append([unwrap_2_list('$5'), ".", unwrap_2_list('$7')]),
                                                                                                                              privilegeType@ => '$3'}}.

packageItemList -> packageItemSimple                      : ['$1'].
packageItemList -> packageItemConditional                 : ['$1'].
packageItemList -> packageItemSimple      packageItemList : ['$1' | '$2'].
packageItemList -> packageItemConditional packageItemList : ['$1' | '$2'].

plsqlPackageSourceAttributeList -> plsqlPackageSourceAttribute                                 : ['$1'].
plsqlPackageSourceAttributeList -> plsqlPackageSourceAttribute plsqlPackageSourceAttributeList : ['$1' | '$2'].

roleAnnotation -> '--<>' ROLE '=' NAME : #{roleAnnotation => #{type@ => unwrap_2_list('$2'),
                                                               role@ => unwrap_2_list('$4')}}.

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

packageItemConditional -> '$ELSE'                     packageItem  '$END' : #{packageItemConditional => #{start@ => unwrap_2_list('$1'),
                                                                                                          packageItem@ => '$2',
                                                                                                          end@ => unwrap_2_list('$3')}}.
packageItemConditional -> '$ELSIF' expression '$THEN' packageItem         : #{packageItemConditional => #{start@ => unwrap_2_list('$1'),
                                                                                                          expression@ => '$2',
                                                                                                          packageItem@ => '$4'}}.
packageItemConditional -> '$ELSIF' expression '$THEN' packageItem  '$END' : #{packageItemConditional => #{start@ => unwrap_2_list('$1'),
                                                                                                          expression@ => '$2',
                                                                                                          packageItem@ => '$4',
                                                                                                          end@ => unwrap_2_list('$5')}}.
packageItemConditional -> '$IF'    expression '$THEN' packageItem         : #{packageItemConditional => #{start@ => unwrap_2_list('$1'),
                                                                                                          expression@ => '$2',
                                                                                                          packageItem@ => '$4'}}.
packageItemConditional -> '$IF'    expression '$THEN' packageItem  '$END' : #{packageItemConditional => #{start@ => unwrap_2_list('$1'),
                                                                                                          expression@ => '$2',
                                                                                                          packageItem@ => '$4',
                                                                                                          end@ => unwrap_2_list('$5')}}.

packageItemSimple -> packageItem : #{packageItemSimple => #{packageItem@ => '$1'}}.

plsqlPackageSourceAttribute -> accessibleByClause     : #{plsqlPackageSourceAttribute => '$1'}.
plsqlPackageSourceAttribute -> defaultCollationClause : #{plsqlPackageSourceAttribute => '$1'}.
plsqlPackageSourceAttribute -> invokerRightsClause    : #{plsqlPackageSourceAttribute => '$1'}.

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

accessibleByClause -> ACCESSIBLE BY accessorCommaList : #{accessibleByClause => #{accessorCommaList@ => '$3'}}.

defaultCollationClause -> DEFAULT COLLATION USING_NLS_COMP : #{defaultCollationClause => lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2'), " ", unwrap_2_list('$3')])}.

invokerRightsClause -> AUTHID CURRENT_USER : #{invokerRightsClause => unwrap_2_list('$2')}.
invokerRightsClause -> AUTHID DEFINER      : #{invokerRightsClause => unwrap_2_list('$2')}.

packageFunctionDeclaration ->                    functionHeading                                         ';' : #{packageFunctionDeclaration  => #{functionHeading@ => '$1'}}.
packageFunctionDeclaration ->                    functionHeading packageFunctionDeclarationAttributeList ';' : #{packageFunctionDeclaration  => #{functionHeading@ => '$1',
                                                                                                                                                  packageFunctionDeclarationAttributeList@ => '$2'}}.
packageFunctionDeclaration -> functionAnnotation functionHeading                                         ';' : #{packageFunctionDeclaration  => #{functionAnnotation@ => '$1',
                                                                                                                                                  functionHeading@ => '$2'}}.
packageFunctionDeclaration -> functionAnnotation functionHeading packageFunctionDeclarationAttributeList ';' : #{packageFunctionDeclaration  => #{functionAnnotation@ => '$1',
                                                                                                                                                  functionHeading@ => '$2',
                                                                                                                                                  packageFunctionDeclarationAttributeList@ => '$3'}}.

packageProcedureDeclaration ->                     procedureHeading                    ';' : #{packageProcedureDeclaration  => #{procedureHeading@ => '$1'}}.
packageProcedureDeclaration ->                     procedureHeading accessibleByClause ';' : #{packageProcedureDeclaration  => #{procedureHeading@ => '$1',
                                                                                                                                 accessibleByClause@ => '$2'}}.
packageProcedureDeclaration -> procedureAnnotation procedureHeading                    ';' : #{packageProcedureDeclaration  => #{procedureAnnotation@ => '$1',
                                                                                                                                 procedureHeading@ => '$2'}}.
packageProcedureDeclaration -> procedureAnnotation procedureHeading accessibleByClause ';' : #{packageProcedureDeclaration  => #{procedureAnnotation@ => '$1',
                                                                                                                                 procedureHeading@ => '$2',
                                                                                                                                 accessibleByClause@ => '$3'}}.

%% Level 08 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

accessorCommaList -> accessor                       : ['$1'].
accessorCommaList -> accessor ',' accessorCommaList : ['$1' | '$3'].

functionAnnotation ->                          privilegeRoleAnnotationList : #{functionAnnotation => #{privilegeRoleAnnotationList@ => '$1'}}.
functionAnnotation -> functionLegacyAnnotation                             : #{functionAnnotation => #{functionLegacyAnnotation@ => '$1'}}.
functionAnnotation -> functionLegacyAnnotation privilegeRoleAnnotationList : #{functionAnnotation => #{functionLegacyAnnotation@ => '$1',
                                                                                                       privilegeRoleAnnotationList@ => '$2'}}.

functionHeading -> FUNCTION NAME                                       RETURN dataType : #{functionHeading => #{name@ => unwrap_2_list('$2'),
                                                                                                                return@ => '$4'}}.
functionHeading -> FUNCTION NAME '(' parameterDeclarationCommaList ')' RETURN dataType : #{functionHeading => #{name@ => unwrap_2_list('$2'),
                                                                                                                parameterDeclarationCommaList@ => '$4',
                                                                                                                return@ => '$7'}}.

packageFunctionDeclarationAttributeList -> packageFunctionDeclarationAttribute                                         : ['$1'].
packageFunctionDeclarationAttributeList -> packageFunctionDeclarationAttribute packageFunctionDeclarationAttributeList : ['$1' | '$2'].

procedureAnnotation ->                                                     privilegeRoleAnnotationList : #{procedureAnnotation => #{privilegeRoleAnnotationList@ => '$1'}}.
procedureAnnotation ->                           procedureLegacyAnnotation                             : #{procedureAnnotation => #{procedureLegacyAnnotation@ => '$1'}}.
procedureAnnotation ->                           procedureLegacyAnnotation privilegeRoleAnnotationList : #{procedureAnnotation => #{procedureLegacyAnnotation@ => '$1',
                                                                                                                                    privilegeRoleAnnotationList@ => '$2'}}.
procedureAnnotation -> functionLegacyAnnotation                                                        : #{procedureAnnotation => #{functionLegacyAnnotation@ => '$1'}}.
procedureAnnotation -> functionLegacyAnnotation                            privilegeRoleAnnotationList : #{procedureAnnotation => #{functionLegacyAnnotation@ => '$1',
                                                                                                                                    privilegeRoleAnnotationList@ => '$2'}}.
procedureAnnotation -> functionLegacyAnnotation  procedureLegacyAnnotation                             : #{procedureAnnotation => #{functionLegacyAnnotation@ => '$1',
                                                                                                                                    procedureLegacyAnnotation@ => '$2'}}.
procedureAnnotation -> functionLegacyAnnotation  procedureLegacyAnnotation privilegeRoleAnnotationList : #{procedureAnnotation => #{functionLegacyAnnotation@ => '$1',
                                                                                                                                    procedureLegacyAnnotation@ => '$2',
                                                                                                                                    privilegeRoleAnnotationList@ => '$3'}}.

procedureHeading -> PROCEDURE NAME                                       : #{procedureHeading => #{name@ => unwrap_2_list('$2')}}.
procedureHeading -> PROCEDURE NAME '(' parameterDeclarationCommaList ')' : #{procedureHeading => #{name@ => unwrap_2_list('$2'),
                                                                                                   parameterDeclarationCommaList@ => '$4'}}.

%% Level 09 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

accessor ->                   NAME : #{accessor => #{name@ => unwrap_2_list('$1')}}.
accessor ->          NAME '.' NAME : #{accessor => #{name@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')])}}.
accessor -> unitKind          NAME : #{accessor => #{unitKind@ => '$1',
                                                     name@ => unwrap_2_list('$2')}}.
accessor -> unitKind NAME '.' NAME : #{accessor => #{unitKind@ => '$1',
                                                     name@ => lists:append([unwrap_2_list('$2'), ".", unwrap_2_list('$4')])}}.

dataType -> BFILE                                                            : #{dataType => #{class@ => sql,
                                                                                               type@ => unwrap_2_list('$1')}}.
dataType -> BINARY_DOUBLE                                                    : #{dataType => #{class@ => sql,
                                                                                               type@ => unwrap_2_list('$1')}}.
dataType -> BINARY_FLOAT                                                     : #{dataType => #{class@ => sql,
                                                                                               type@ => unwrap_2_list('$1')}}.
dataType -> BINARY_INTEGER                                                   : #{dataType => #{class@ => plsql,
                                                                                               type@ => unwrap_2_list('$1')}}.
dataType -> BLOB                                                             : #{dataType => #{class@ => sql,
                                                                                               type@ => unwrap_2_list('$1')}}.
dataType -> BOOLEAN                                                          : #{dataType => #{class@ => plsql,
                                                                                               type@ => unwrap_2_list('$1')}}.
dataType -> CHAR                                                             : #{dataType => #{class@ => sql,
                                                                                               type@ => unwrap_2_list('$1')}}.
dataType -> CHAR          '(' INTNUM            ')'                          : #{dataType => #{class@ => sql,
                                                                                               type@ => unwrap_2_list('$1'),
                                                                                               size@ => unwrap_2_list('$3')}}.
dataType -> CHAR          '(' INTNUM     BYTE   ')'                          : #{dataType => #{class@ => sql,
                                                                                               type@ => unwrap_2_list('$1'),
                                                                                               size@ => unwrap_2_list('$3'),
                                                                                               sizeType@ => unwrap_2_list('$4')}}.
dataType -> CHAR          '(' INTNUM     CHAR   ')'                          : #{dataType => #{class@ => sql,
                                                                                               type@ => unwrap_2_list('$1'),
                                                                                               size@ => unwrap_2_list('$3'),
                                                                                               sizeType@ => unwrap_2_list('$4')}}.
dataType -> CLOB                                                             : #{dataType => #{class@ => sql,
                                                                                               type@ => unwrap_2_list('$1')}}.
dataType -> DATE                                                             : #{dataType => #{class@ => sql,
                                                                                               type@ => unwrap_2_list('$1')}}.
dataType -> FLOAT                                                            : #{dataType => #{class@ => sql,
                                                                                               type@ => unwrap_2_list('$1')}}.
dataType -> FLOAT         '(' INTNUM            ')'                          : #{dataType => #{class@ => sql,
                                                                                               type@ => unwrap_2_list('$1'),
                                                                                               precision@ =>  unwrap_2_list('$3')}}.
dataType -> INTERVAL DAY                            TO SECOND                : #{dataType => #{class@ => sql,
                                                                                               type@ => "INTERVAL DAY"}}.
dataType -> INTERVAL DAY  '(' INTNUM            ')' TO SECOND                : #{dataType => #{class@ => sql,
                                                                                               type@ => "INTERVAL DAY",
                                                                                               dayPrecision@ => unwrap_2_list('$4')}}.
dataType -> INTERVAL DAY                            TO SECOND '(' INTNUM ')' : #{dataType => #{class@ => sql,
                                                                                               type@ => "INTERVAL DAY",
                                                                                               secondPrecision@ => unwrap_2_list('$6')}}.
dataType -> INTERVAL DAY  '(' INTNUM            ')' TO SECOND '(' INTNUM ')' : #{dataType => #{class@ => sql,
                                                                                               type@ => "INTERVAL DAY",
                                                                                               dayPrecision@ => unwrap_2_list('$4'),
                                                                                               secondPrecision@ => unwrap_2_list('$9')}}.
dataType -> INTERVAL YEAR                           TO MONTH                 : #{dataType => #{class@ => sql,
                                                                                               type@ => "INTERVAL YEAR"}}.
dataType -> INTERVAL YEAR '(' INTNUM            ')' TO MONTH                 : #{dataType => #{class@ => sql,
                                                                                               type@ => "INTERVAL YEAR",
                                                                                               precision@ => unwrap_2_list('$4')}}.
dataType -> LONG RAW                                                         : #{dataType => #{class@ => sql,
                                                                                               type@ => "LONG RAW"}}.
dataType -> NAME                                                             : #{dataType => #{class@ => user_defined,
                                                                                               type@ => unwrap_2_list('$1')}}.
dataType -> NAME                   '%ROWTYPE'                                : #{dataType => #{class@ => user_defined,
                                                                                               type@ => unwrap_2_list('$1'),
                                                                                               attribute@ => unwrap_2_list('$2')}}.
dataType -> NAME                   '%TYPE'                                   : #{dataType => #{class@ => user_defined,
                                                                                               type@ => unwrap_2_list('$1'),
                                                                                               attribute@ => unwrap_2_list('$2')}}.
dataType -> NAME '.' NAME                                                    : #{dataType => #{class@ => user_defined,
                                                                                               type@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')])}}.
dataType -> NAME '.' NAME          '%TYPE'                                   : #{dataType => #{class@ => user_defined,
                                                                                               type@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')]),
                                                                                               attribute@ => unwrap_2_list('$4')}}.
dataType -> NAME '.' NAME '.' NAME                                           : #{dataType => #{class@ => user_defined,
                                                                                               type@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3'), ".", unwrap_2_list('$5')])}}.
dataType -> NAME '.' NAME '.' NAME '%TYPE'                                   : #{dataType => #{class@ => user_defined,
                                                                                               type@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3'), ".", unwrap_2_list('$5')]),
                                                                                               attribute@ => unwrap_2_list('$6')}}.
dataType -> NCHAR         '(' INTNUM            ')'                          : #{dataType => #{class@ => sql,
                                                                                               type@ => unwrap_2_list('$1'),
                                                                                               size@ => unwrap_2_list('$3')}}.
dataType -> NCLOB                                                            : #{dataType => #{class@ => sql,
                                                                                               type@ => unwrap_2_list('$1')}}.
dataType -> NUMBER                                                           : #{dataType => #{class@ => sql,
                                                                                               type@ => unwrap_2_list('$1')}}.
dataType -> NUMBER        '(' INTNUM            ')'                          : #{dataType => #{class@ => sql,
                                                                                               type@ => unwrap_2_list('$1'),
                                                                                               precision@ => unwrap_2_list('$3')}}.
dataType -> NUMBER        '(' INTNUM ',' INTNUM ')'                          : #{dataType => #{class@ => sql,
                                                                                               type@ => unwrap_2_list('$1'),
                                                                                               precision@ => unwrap_2_list('$3'),
                                                                                               scale@ =>  unwrap_2_list('$5')}}.
dataType -> NVARCHAR2     '(' INTNUM            ')'                          : #{dataType => #{class@ => sql,
                                                                                               type@ => unwrap_2_list('$1'),
                                                                                               size@ => unwrap_2_list('$3')}}.
dataType -> PLS_INTEGER                                                      : #{dataType => #{class@ => plsql,
                                                                                               type@ => unwrap_2_list('$1')}}.
dataType -> RAW           '(' INTNUM            ')'                          : #{dataType => #{class@ => sql,
                                                                                               type@ => unwrap_2_list('$1'),
                                                                                               size@ => unwrap_2_list('$3')}}.
dataType -> REF CURSOR                                                       : #{dataType => #{class@ => plsql,
                                                                                               type@ => "REF CURSOR"}}.
dataType -> ROWID                                                            : #{dataType => #{class@ => sql,
                                                                                               type@ => unwrap_2_list('$1')}}.
dataType -> TIMESTAMP                                                        : #{dataType => #{class@ => sql,
                                                                                               type@ => unwrap_2_list('$1')}}.
dataType -> TIMESTAMP                               WITH       TIME ZONE     : #{dataType => #{class@ => sql,
                                                                                               type@ => unwrap_2_list('$1'),
                                                                                               timeZone@ => true}}.
dataType -> TIMESTAMP                               WITH LOCAL TIME ZONE     : #{dataType => #{class@ => sql,
                                                                                               type@ => unwrap_2_list('$1'),
                                                                                               timeZone@ => true,
                                                                                               local@ => true}}.
dataType -> TIMESTAMP     '(' INTNUM            ')'                          : #{dataType => #{class@ => sql,
                                                                                               type@ => unwrap_2_list('$1'),
                                                                                               precision@ => unwrap_2_list('$3')}}.
dataType -> TIMESTAMP     '(' INTNUM            ')' WITH       TIME ZONE     : #{dataType => #{class@ => sql,
                                                                                               type@ => unwrap_2_list('$1'),
                                                                                               precision@ => unwrap_2_list('$3'),
                                                                                               timeZone@ => true}}.
dataType -> TIMESTAMP     '(' INTNUM            ')' WITH LOCAL TIME ZONE     : #{dataType => #{class@ => sql,
                                                                                               type@ => unwrap_2_list('$1'),
                                                                                               precision@ => unwrap_2_list('$3'),
                                                                                               timeZone@ => true,
                                                                                               local@ => true}}.
dataType -> UROWID                                                           : #{dataType => #{class@ => sql,
                                                                                               type@ => unwrap_2_list('$1')}}.
dataType -> UROWID        '(' INTNUM            ')'                          : #{dataType => #{class@ => sql,
                                                                                               type@ => unwrap_2_list('$1'),
                                                                                               size@ => unwrap_2_list('$3')}}.
dataType -> VARCHAR2                                                         : #{dataType => #{class@ => sql,
                                                                                               type@ => unwrap_2_list('$1')}}.
dataType -> VARCHAR2      '(' INTNUM            ')'                          : #{dataType => #{class@ => sql,
                                                                                               type@ => unwrap_2_list('$1'),
                                                                                               size@ => unwrap_2_list('$3')}}.
dataType -> VARCHAR2      '(' INTNUM BYTE       ')'                          : #{dataType => #{class@ => sql,
                                                                                               type@ => unwrap_2_list('$1'),
                                                                                               size@ => unwrap_2_list('$3'),
                                                                                               sizeType@ => unwrap_2_list('$4')}}.
dataType -> VARCHAR2      '(' INTNUM CHAR       ')'                          : #{dataType => #{class@ => sql,
                                                                                               type@ => unwrap_2_list('$1'),
                                                                                               size@ => unwrap_2_list('$3'),
                                                                                               sizeType@ => unwrap_2_list('$4')}}.
dataType -> XMLTYPE                                                          : #{dataType => #{class@ => sql,
                                                                                               type@ => unwrap_2_list('$1')}}.

functionLegacyAnnotation -> '--<>' LEGACY_NAME_FUNCTION '=' NAME : #{functionLegacyAnnotation => #{type@ => unwrap_2_list('$2'),
                                                                                                   value@ => unwrap_2_list('$4')}}.

packageFunctionDeclarationAttribute -> accessibleByClause    : #{packageFunctionDeclarationAttribute => '$1'}.
packageFunctionDeclarationAttribute -> DETERMINISTIC         : #{packageFunctionDeclarationAttribute => unwrap_2_list('$1')}.
packageFunctionDeclarationAttribute -> parallelEnabledClause : #{packageFunctionDeclarationAttribute => '$1'}.
packageFunctionDeclarationAttribute -> pipelinedClause       : #{packageFunctionDeclarationAttribute => '$1'}.
packageFunctionDeclarationAttribute -> resultCacheClause     : #{packageFunctionDeclarationAttribute => '$1'}.

parameterDeclarationCommaList -> parameterDeclarationHelper                                   : ['$1'].
parameterDeclarationCommaList -> parameterDeclarationHelper ',' parameterDeclarationCommaList : ['$1' | '$3'].

parameterDeclarationHelper ->                     parameterDeclaration : #{parameterDeclarationHelper => #{parameterDeclaration@ => '$1'}}.
parameterDeclarationHelper -> parameterAnnotation parameterDeclaration : #{parameterDeclarationHelper => #{parameterAnnotation@ => '$1',
                                                                                                           parameterDeclaration@ => '$2'}}.

procedureLegacyAnnotation -> '--<>' LEGACY_NAME_PROCEDURE '=' NAME : #{procedureLegacyAnnotation => #{type@ => unwrap_2_list('$2'),
                                                                                                      value@ => unwrap_2_list('$4')}}.

%% Level 10 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parameterAnnotation -> '--<>' LOGGER_TO_CHARACTER '=' FALSE : #{parameterAnnotation => #{type@ => unwrap_2_list('$2'),
                                                                                         value@ => unwrap_2_list('$4')}}.
parameterAnnotation -> '--<>' LOGGER_TO_CHARACTER '=' NONE  : #{parameterAnnotation => #{type@ => unwrap_2_list('$2'),
                                                                                         value@ => unwrap_2_list('$4')}}.

parameterDeclaration -> NAME               dataType         : #{parameterDeclaration => #{name@ => unwrap_2_list('$1'),
                                                                                          dataType@ => '$2'}}.
parameterDeclaration -> NAME               dataType default : #{parameterDeclaration => #{name@ => unwrap_2_list('$1'),
                                                                                          dataType@ => '$2',
                                                                                          default@ => '$3'}}.
parameterDeclaration -> NAME IN            dataType         : #{parameterDeclaration => #{name@ => unwrap_2_list('$1'),
                                                                                          mode@ => unwrap_2_list('$2'),
                                                                                          dataType@ => '$3'}}.
parameterDeclaration -> NAME IN            dataType default : #{parameterDeclaration => #{name@ => unwrap_2_list('$1'),
                                                                                          mode@ => unwrap_2_list('$2'),
                                                                                          dataType@ => '$3',
                                                                                          default@ => '$4'}}.
parameterDeclaration -> NAME    OUT        dataType         : #{parameterDeclaration => #{name@ => unwrap_2_list('$1'),
                                                                                          mode@ => unwrap_2_list('$2'),
                                                                                          dataType@ => '$3'}}.
parameterDeclaration -> NAME    OUT NOCOPY dataType         : #{parameterDeclaration => #{name@ => unwrap_2_list('$1'),
                                                                                          mode@ => unwrap_2_list('$2'),
                                                                                          nocopy@ => unwrap_2_list('$3'),
                                                                                          dataType@ => '$4'}}.
parameterDeclaration -> NAME IN OUT        dataType         : #{parameterDeclaration => #{name@ => unwrap_2_list('$1'),
                                                                                          mode@ => "IN OUT",
                                                                                          dataType@ => '$4'}}.
parameterDeclaration -> NAME IN OUT NOCOPY dataType         : #{parameterDeclaration => #{name@ => unwrap_2_list('$1'),
                                                                                          mode@ => "IN OUT",
                                                                                          nocopy@ => unwrap_2_list('$4'),
                                                                                          dataType@ => '$5'}}.

unitKind -> FUNCTION  : unwrap_2_list('$1').
unitKind -> PACKAGE   : unwrap_2_list('$1').
unitKind -> PROCEDURE : unwrap_2_list('$1').
unitKind -> TRIGGER   : unwrap_2_list('$1').
unitKind -> TYPE      : unwrap_2_list('$1').

%% Level 11 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

default -> ':='    expression : #{default => #{type@ => unwrap_2_list('$1'),
                                               value@ => '$2'}}.
default -> DEFAULT expression : #{default => #{type@ => unwrap_2_list('$1'),
                                               value@ => '$2'}}.

expression -> columnRef                        : #{expression => '$1'}.
expression -> functionRef                      : #{expression => '$1'}.
expression -> literal                          : #{expression => '$1'}.
expression -> NULLX                            : #{expression => 'NULL'}.
expression -> parameterRef                     : #{expression => '$1'}.
expression -> '(' expression ')'               : #{expression => #{operator@ => '(',
                                                                   expression@ => '$2'}}.
expression -> 'NOT'                 expression : #{expression => #{operator@ => 'NOT',
                                                                   expression@ => '$2'}}.
expression -> unaryAddOrSubtract    expression : #{expression => #{operator@ => '$1',
                                                                   expression@ => '$2'}}.
expression -> expression 'AND'      expression : #{expression => #{operator@ => 'AND',
                                                                   expressionLeft@ => '$1',
                                                                   expressionRight@ => '$3'}}.
expression -> expression 'OR'       expression : #{expression => #{operator@ => 'OR',
                                                                   expressionLeft@ => '$1',
                                                                   expressionRight@ => '$3'}}.
expression -> expression '+'        expression : #{expression => #{operator@ => '+',
                                                                   expressionLeft@ => '$1',
                                                                   expressionRight@ => '$3'}}.
expression -> expression '-'        expression : #{expression => #{operator@ => '-',
                                                                   expressionLeft@ => '$1',
                                                                   expressionRight@ => '$3'}}.
expression -> expression '/'        expression : #{expression => #{operator@ => '/',
                                                                   expressionLeft@ => '$1',
                                                                   expressionRight@ => '$3'}}.
expression -> expression '*'        expression : #{expression => #{operator@ => '*',
                                                                   expressionLeft@ => '$1',
                                                                   expressionRight@ => '$3'}}.
expression -> expression '||'       expression : #{expression => #{operator@ => '||',
                                                                   expressionLeft@ => '$1',
                                                                   expressionRight@ => '$3'}}.
expression -> expression '='        expression : #{expression => #{operator@ => '=',
                                                                   expressionLeft@ => '$1',
                                                                   expressionRight@ => '$3'}}.
expression -> expression COMPARISON expression : #{expression => #{operator@ => unwrap_2_atom('$2'),
                                                                   expressionLeft@ => '$1',
                                                                   expressionRight@ => '$3'}}.

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
pipelinedClause -> PIPELINED                   USING NAME '.' NAME : #{pipelinedClause => #{implementationPackage@ => lists:append([unwrap_2_list('$3'), ".", unwrap_2_list('$5')])}}.
pipelinedClause -> PIPELINED ROW   POLYMORPHIC USING          NAME : #{pipelinedClause => #{type@ => unwrap_2_list('$2'),
                                                                                            implementationPackage@ => unwrap_2_list('$5')}}.
pipelinedClause -> PIPELINED ROW   POLYMORPHIC USING NAME '.' NAME : #{pipelinedClause => #{type@ => unwrap_2_list('$2'),
                                                                                            implementationPackage@ => lists:append([unwrap_2_list('$5'), ".", unwrap_2_list('$7')])}}.
pipelinedClause -> PIPELINED TABLE POLYMORPHIC USING          NAME : #{pipelinedClause => #{type@ => unwrap_2_list('$2'),
                                                                                            implementationPackage@ => unwrap_2_list('$5')}}.
pipelinedClause -> PIPELINED TABLE POLYMORPHIC USING NAME '.' NAME : #{pipelinedClause => #{type@ => unwrap_2_list('$2'),
                                                                                            implementationPackage@ => lists:append([unwrap_2_list('$5'), ".", unwrap_2_list('$7')])}}.

resultCacheClause -> RESULT_CACHE                                       : #{resultCacheClause => #{dataSourceCommaList@ => {}}}.
resultCacheClause -> RESULT_CACHE RELIES_ON '(' dataSourceCommaList ')' : #{resultCacheClause => #{dataSourceCommaList@ => '$4'}}.

%% Level 12 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

columnRef -> NAME                               : #{columnRef => unwrap_2_list('$1')}.
columnRef -> NAME '.' NAME                      : #{columnRef => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')])}.
columnRef -> NAME '.' NAME '.' NAME             : #{columnRef => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3'), ".", unwrap_2_list('$5')])}.
columnRef -> NAME '(' '+' ')'                   : #{columnRef => lists:append([unwrap_2_list('$1'), "(+)"])}.
columnRef -> NAME '.' NAME '(' '+' ')'          : #{columnRef => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3'), "(+)"])}.
columnRef -> NAME '.' NAME '.' NAME '(' '+' ')' : #{columnRef => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3'), ".", unwrap_2_list('$5'),"(+)"])}.
columnRef -> NAME '.' '*'                       : #{columnRef => lists:append([unwrap_2_list('$1'), ".*"])}.
columnRef -> NAME '.' NAME '.' '*'              : #{columnRef => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3'), ".*"])}.

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

functionRef -> NAME                   '('                      ')' : #{functionRef => #{name@ => unwrap_2_list('$1'),
                                                                                        functionArgCommaList@ => '()'}}.
functionRef -> NAME                   '(' functionArgCommaList ')' : #{functionRef => #{name@ => unwrap_2_list('$1'),
                                                                                        functionArgCommaList@ => make_list('$3')}}.
functionRef -> NAME '.' NAME          '('                      ')' : #{functionRef => #{name@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')]),
                                                                                        functionArgCommaList@ => '()'}}.
functionRef -> NAME '.' NAME          '(' functionArgCommaList ')' : #{functionRef => #{name@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')]),
                                                                                        functionArgCommaList@ => make_list('$5')}}.
functionRef -> NAME '.' NAME '.' NAME '('                      ')' : #{functionRef => #{name@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3'), ".", unwrap_2_list('$5')]),
                                                                                        functionArgCommaList@ => '()'}}.
functionRef -> NAME '.' NAME '.' NAME '(' functionArgCommaList ')' : #{functionRef => #{name@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3'), ".", unwrap_2_list('$5')]),
                                                                                        functionArgCommaList@ => make_list('$7')}}.

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

unaryAddOrSubtract -> '+' : #{unaryAddOrSubtract => unwrap_2_list('$1')}.
unaryAddOrSubtract -> '-' : #{unaryAddOrSubtract => unwrap_2_list('$1')}.

%% Level 15 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

functionArgCommaList -> functionArg                          : ['$1'].
functionArgCommaList -> functionArg ',' functionArgCommaList : ['$1' | '$3'].

%% Level 16 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

functionArg -> expression            : #{functionArg => '$1'}.
functionArg -> NAME '=>' expression  : #{functionArg => #{name@ => unwrap_2_list('$1'),
                                                          expression@ => '$3'}}.

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

make_list(L) when is_list(L) ->
    L;
make_list(L) ->
    [L].

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
