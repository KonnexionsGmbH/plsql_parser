%% -----------------------------------------------------------------------------
%%
%% plsql_parser.yrl: PL/SQL - parser definition.
%%
%% Copyright (c) 2018-20 Konnexions GmbH.  All Rights Reserved.
%%
%% -----------------------------------------------------------------------------

%% -*- erlang -*-
Header "%% Copyright (C) Konnexions GmbH"
"%% @private"
"%% @Author Walter Weinmann"
"%% @Email walter@konnexions.ch".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Nonterminals
 accessibleByClause
 accessor
 accessorCommaList
 apiGroupAnnotation
 apiHiddenAnnotation
 as_is
 assocArrayTypeDef
 collectionTypeDefinition
 columnRef
 columnRefCommaList
 constantDeclaration
 createPackage
 dataSource
 dataSourceCommaList
 dataType_1
 dataType_2
 dataType_3
 default
 defaultCollationClause
 exceptionDeclaration
 expression
 fieldDefinition
 fieldDefinitionCommaList
 functionAnnotation
 functionArg
 functionArgCommaList
 functionHeading
 functionRef
 invokerRightsClause
 itemDeclaration
 literal
 nameExtended
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
 parameterDeclaration
 parameterDeclarationCommaList
 parameterRef
 pipelinedClause
 plsqlPackageSource
 plsqlPackageSourceAttribute
 plsqlPackageSourceAttributeList
 plsqlScript
 plsqlUnit
 pragmaDeclaration
 pragmaParameterExceptionInit
 pragmaParameterRestrictReferences
 privilegeAnnotationList
 procedureAnnotation
 procedureHeading
 recordTypeDefinition
 refCursorTypeDefinition
 restrictReferencesList
 resultCacheClause
 sharingClause
 sqlplusCommand
 streamingClause
 subtypeDefinition
 systemPrivilegeAnnotation
 systemPrivilegeType
 typeDefinition
 unaryAddOrSubtract
 unitKind
 variableDeclaration
 varrayTypeDef
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
 AND
 ANY
 API_GROUP
 API_HIDDEN
 APPROXNUM
 ARRAY
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
 CONSTANT
 CONTAINER
 CREATE
 CURRENT_USER
 CURSOR
 DATE
 DAY
 DEFAULT
 DEFINE
 DEFINER
 DETERMINISTIC
 EDITIONABLE
 END
 EXCEPTION
 EXCEPTION_INIT
 FALSE
 FLOAT
 FUNCTION
 HASH
 IN
 INDEX
 INDICATOR
 INTERVAL
 INTNUM
 IS
 LOCAL
 LONG
 MAN_PAGE
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
 OF
 OFF
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
 PRAGMA
 PROCEDURE
 RANGE
 RAW
 RECORD
 REF
 REFRESH
 RELIES_ON
 REPLACE
 RESTRICT_REFERENCES
 RESULT_CACHE
 RETURN
 RNDS
 RNPS
 ROW
 ROWID
 SECOND
 SERIALLY_REUSABLE
 SET
 SHARING
 STRING
 SUBTYPE
 SYSTEM_PRIVILEGE
 TABLE
 TIME
 TIMESTAMP
 TO
 TRIGGER
 TRUE
 TRUST
 TYPE
 UDF
 UROWID
 USING
 USING_NLS_COMP
 VALUE
 VARCHAR2
 VARRAY
 VARYING
 WITH
 WNDS
 WNPS
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

plsqlUnit ->                createPackage : #{plsqlUnit => #{                         createPackage@ => '$1'}}.
plsqlUnit -> sqlplusCommand createPackage : #{plsqlUnit => #{sqlplusCommand@ => '$1', createPackage@ => '$2'}}.

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

sqlplusCommand -> SET DEFINE OFF ';' : #{sqlplusCommand => lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2'), " ", unwrap_2_list('$3')])}.
sqlplusCommand -> SET DEFINE ON  ';' : #{sqlplusCommand => lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2'), " ", unwrap_2_list('$3')])}.

%% Level 04 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

plsqlPackageSource ->          NAME                                               as_is                          END      : #{plsqlPackageSource => #{asIs@ => '$2',
                                                                                                                                                      packageNameStart@ => unwrap_2_list('$1')}}.
plsqlPackageSource ->          NAME                                               as_is                          END NAME : #{plsqlPackageSource => #{asIs@ => '$2',
                                                                                                                                                      packageNameEnd@ => unwrap_2_list('$4'),
                                                                                                                                                      packageNameStart@ => unwrap_2_list('$1')}}.
plsqlPackageSource ->          NAME                                               as_is          packageItemList END      : #{plsqlPackageSource => #{asIs@ => '$2',
                                                                                                                                                      packageItemList@ => '$3',
                                                                                                                                                      packageNameStart@ => unwrap_2_list('$1')}}.
plsqlPackageSource ->          NAME                                               as_is          packageItemList END NAME : #{plsqlPackageSource => #{asIs@ => '$2',
                                                                                                                                                      packageItemList@ => '$3',
                                                                                                                                                      packageNameEnd@ => unwrap_2_list('$5'),
                                                                                                                                                      packageNameStart@ => unwrap_2_list('$1')}}.
plsqlPackageSource ->          NAME                                               as_is MAN_PAGE                 END      : #{plsqlPackageSource => #{asIs@ => '$2',
                                                                                                                                                      man_page@ => unwrap_2_list('$3'),
                                                                                                                                                      packageNameStart@ => unwrap_2_list('$1')}}.
plsqlPackageSource ->          NAME                                               as_is MAN_PAGE                 END NAME : #{plsqlPackageSource => #{asIs@ => '$2',
                                                                                                                                                      man_page@ => unwrap_2_list('$3'),
                                                                                                                                                      packageNameEnd@ => unwrap_2_list('$5'),
                                                                                                                                                      packageNameStart@ => unwrap_2_list('$1')}}.
plsqlPackageSource ->          NAME                                               as_is MAN_PAGE packageItemList END      : #{plsqlPackageSource => #{asIs@ => '$2',
                                                                                                                                                      man_page@ => unwrap_2_list('$3'),
                                                                                                                                                      packageItemList@ => '$4',
                                                                                                                                                      packageNameStart@ => unwrap_2_list('$1')}}.
plsqlPackageSource ->          NAME                                               as_is MAN_PAGE packageItemList END NAME : #{plsqlPackageSource => #{asIs@ => '$2',
                                                                                                                                                      man_page@ => unwrap_2_list('$3'),
                                                                                                                                                      packageItemList@ => '$4',
                                                                                                                                                      packageNameEnd@ => unwrap_2_list('$6'),
                                                                                                                                                      packageNameStart@ => unwrap_2_list('$1')}}.
plsqlPackageSource ->          NAME               plsqlPackageSourceAttributeList as_is                          END      : #{plsqlPackageSource => #{asIs@ => '$3',
                                                                                                                                                      packageNameStart@ => unwrap_2_list('$1'),
                                                                                                                                                      plsqlPackageSourceAttributeList@ => '$2'}}.
plsqlPackageSource ->          NAME               plsqlPackageSourceAttributeList as_is                          END NAME : #{plsqlPackageSource => #{asIs@ => '$3',
                                                                                                                                                      packageNameEnd@ => unwrap_2_list('$5'),
                                                                                                                                                      packageNameStart@ => unwrap_2_list('$1'),
                                                                                                                                                      plsqlPackageSourceAttributeList@ => '$2'}}.
plsqlPackageSource ->          NAME               plsqlPackageSourceAttributeList as_is          packageItemList END      : #{plsqlPackageSource => #{asIs@ => '$3',
                                                                                                                                                      packageItemList@ => '$4',
                                                                                                                                                      packageNameStart@ => unwrap_2_list('$1'),
                                                                                                                                                      plsqlPackageSourceAttributeList@ => '$2'}}.
plsqlPackageSource ->          NAME               plsqlPackageSourceAttributeList as_is          packageItemList END NAME : #{plsqlPackageSource => #{asIs@ => '$3',
                                                                                                                                                      packageItemList@ => '$4',
                                                                                                                                                      packageNameEnd@ => unwrap_2_list('$6'),
                                                                                                                                                      packageNameStart@ => unwrap_2_list('$1'),
                                                                                                                                                      plsqlPackageSourceAttributeList@ => '$2'}}.
plsqlPackageSource ->          NAME               plsqlPackageSourceAttributeList as_is MAN_PAGE                 END      : #{plsqlPackageSource => #{asIs@ => '$3',
                                                                                                                                                      man_page@ => unwrap_2_list('$4'),
                                                                                                                                                      packageNameStart@ => unwrap_2_list('$1'),
                                                                                                                                                      plsqlPackageSourceAttributeList@ => '$2'}}.
plsqlPackageSource ->          NAME               plsqlPackageSourceAttributeList as_is MAN_PAGE                 END NAME : #{plsqlPackageSource => #{asIs@ => '$3',
                                                                                                                                                      man_page@ => unwrap_2_list('$4'),
                                                                                                                                                      packageNameEnd@ => unwrap_2_list('$6'),
                                                                                                                                                      packageNameStart@ => unwrap_2_list('$1'),
                                                                                                                                                      plsqlPackageSourceAttributeList@ => '$2'}}.
plsqlPackageSource ->          NAME               plsqlPackageSourceAttributeList as_is MAN_PAGE packageItemList END      : #{plsqlPackageSource => #{asIs@ => '$3',
                                                                                                                                                      man_page@ => unwrap_2_list('$4'),
                                                                                                                                                      packageItemList@ => '$5',
                                                                                                                                                      packageNameStart@ => unwrap_2_list('$1'),
                                                                                                                                                      plsqlPackageSourceAttributeList@ => '$2'}}.
plsqlPackageSource ->          NAME               plsqlPackageSourceAttributeList as_is MAN_PAGE packageItemList END NAME : #{plsqlPackageSource => #{asIs@ => '$3',
                                                                                                                                                      man_page@ => unwrap_2_list('$4'),
                                                                                                                                                      packageItemList@ => '$5',
                                                                                                                                                      packageNameEnd@ => unwrap_2_list('$7'),
                                                                                                                                                      packageNameStart@ => unwrap_2_list('$1'),
                                                                                                                                                      plsqlPackageSourceAttributeList@ => '$2'}}.
plsqlPackageSource ->          NAME sharingClause                                 as_is                          END      : #{plsqlPackageSource => #{asIs@ => '$3',
                                                                                                                                                      packageNameStart@ => unwrap_2_list('$1'),
                                                                                                                                                      sharingClause@ => '$2'}}.
plsqlPackageSource ->          NAME sharingClause                                 as_is                          END NAME : #{plsqlPackageSource => #{asIs@ => '$3',
                                                                                                                                                      packageNameEnd@ => unwrap_2_list('$5'),
                                                                                                                                                      packageNameStart@ => unwrap_2_list('$1'),
                                                                                                                                                      sharingClause@ => '$2'}}.
plsqlPackageSource ->          NAME sharingClause                                 as_is          packageItemList END      : #{plsqlPackageSource => #{asIs@ => '$3',
                                                                                                                                                      packageItemList@ => '$4',
                                                                                                                                                      packageNameStart@ => unwrap_2_list('$1'),
                                                                                                                                                      sharingClause@ => '$2'}}.
plsqlPackageSource ->          NAME sharingClause                                 as_is          packageItemList END NAME : #{plsqlPackageSource => #{asIs@ => '$3',
                                                                                                                                                      packageItemList@ => '$4',
                                                                                                                                                      packageNameEnd@ => unwrap_2_list('$6'),
                                                                                                                                                      packageNameStart@ => unwrap_2_list('$1'),
                                                                                                                                                      sharingClause@ => '$2'}}.
plsqlPackageSource ->          NAME sharingClause                                 as_is MAN_PAGE                 END      : #{plsqlPackageSource => #{asIs@ => '$3',
                                                                                                                                                      man_page@ => unwrap_2_list('$4'),
                                                                                                                                                      packageNameStart@ => unwrap_2_list('$1'),
                                                                                                                                                      sharingClause@ => '$2'}}.
plsqlPackageSource ->          NAME sharingClause                                 as_is MAN_PAGE                 END NAME : #{plsqlPackageSource => #{asIs@ => '$3',
                                                                                                                                                      man_page@ => unwrap_2_list('$4'),
                                                                                                                                                      packageNameEnd@ => unwrap_2_list('$6'),
                                                                                                                                                      packageNameStart@ => unwrap_2_list('$1'),
                                                                                                                                                      sharingClause@ => '$2'}}.
plsqlPackageSource ->          NAME sharingClause                                 as_is MAN_PAGE packageItemList END      : #{plsqlPackageSource => #{asIs@ => '$3',
                                                                                                                                                      man_page@ => unwrap_2_list('$4'),
                                                                                                                                                      packageItemList@ => '$5',
                                                                                                                                                      packageNameStart@ => unwrap_2_list('$1'),
                                                                                                                                                      sharingClause@ => '$2'}}.
plsqlPackageSource ->          NAME sharingClause                                 as_is MAN_PAGE packageItemList END NAME : #{plsqlPackageSource => #{asIs@ => '$3',
                                                                                                                                                      man_page@ => unwrap_2_list('$4'),
                                                                                                                                                      packageItemList@ => '$5',
                                                                                                                                                      packageNameEnd@ => unwrap_2_list('$7'),
                                                                                                                                                      packageNameStart@ => unwrap_2_list('$1'),
                                                                                                                                                      sharingClause@ => '$2'}}.
plsqlPackageSource ->          NAME sharingClause plsqlPackageSourceAttributeList as_is                          END      : #{plsqlPackageSource => #{asIs@ => '$4',
                                                                                                                                                      packageNameStart@ => unwrap_2_list('$1'),
                                                                                                                                                      plsqlPackageSourceAttributeList@ => '$3',
                                                                                                                                                      sharingClause@ => '$2'}}.
plsqlPackageSource ->          NAME sharingClause plsqlPackageSourceAttributeList as_is                          END NAME : #{plsqlPackageSource => #{asIs@ => '$4',
                                                                                                                                                      packageNameEnd@ => unwrap_2_list('$6'),
                                                                                                                                                      packageNameStart@ => unwrap_2_list('$1'),
                                                                                                                                                      plsqlPackageSourceAttributeList@ => '$3',
                                                                                                                                                      sharingClause@ => '$2'}}.
plsqlPackageSource ->          NAME sharingClause plsqlPackageSourceAttributeList as_is          packageItemList END      : #{plsqlPackageSource => #{asIs@ => '$4',
                                                                                                                                                      packageItemList@ => '$5',
                                                                                                                                                      packageNameStart@ => unwrap_2_list('$1'),
                                                                                                                                                      plsqlPackageSourceAttributeList@ => '$3',
                                                                                                                                                      sharingClause@ => '$2'}}.
plsqlPackageSource ->          NAME sharingClause plsqlPackageSourceAttributeList as_is          packageItemList END NAME : #{plsqlPackageSource => #{asIs@ => '$4',
                                                                                                                                                      packageItemList@ => '$5',
                                                                                                                                                      packageNameEnd@ => unwrap_2_list('$7'),
                                                                                                                                                      packageNameStart@ => unwrap_2_list('$1'),
                                                                                                                                                      plsqlPackageSourceAttributeList@ => '$3',
                                                                                                                                                      sharingClause@ => '$2'}}.
plsqlPackageSource ->          NAME sharingClause plsqlPackageSourceAttributeList as_is MAN_PAGE                 END      : #{plsqlPackageSource => #{asIs@ => '$4',
                                                                                                                                                      man_page@ => unwrap_2_list('$5'),
                                                                                                                                                      packageNameStart@ => unwrap_2_list('$1'),
                                                                                                                                                      plsqlPackageSourceAttributeList@ => '$3',
                                                                                                                                                      sharingClause@ => '$2'}}.
plsqlPackageSource ->          NAME sharingClause plsqlPackageSourceAttributeList as_is MAN_PAGE                 END NAME : #{plsqlPackageSource => #{asIs@ => '$4',
                                                                                                                                                      man_page@ => unwrap_2_list('$5'),
                                                                                                                                                      packageNameEnd@ => unwrap_2_list('$7'),
                                                                                                                                                      packageNameStart@ => unwrap_2_list('$1'),
                                                                                                                                                      plsqlPackageSourceAttributeList@ => '$3',
                                                                                                                                                      sharingClause@ => '$2'}}.
plsqlPackageSource ->          NAME sharingClause plsqlPackageSourceAttributeList as_is MAN_PAGE packageItemList END      : #{plsqlPackageSource => #{asIs@ => '$4',
                                                                                                                                                      man_page@ => unwrap_2_list('$5'),
                                                                                                                                                      packageItemList@ => '$6',
                                                                                                                                                      packageNameStart@ => unwrap_2_list('$1'),
                                                                                                                                                      plsqlPackageSourceAttributeList@ => '$3',
                                                                                                                                                      sharingClause@ => '$2'}}.
plsqlPackageSource ->          NAME sharingClause plsqlPackageSourceAttributeList as_is MAN_PAGE packageItemList END NAME : #{plsqlPackageSource => #{asIs@ => '$4',
                                                                                                                                                      man_page@ => unwrap_2_list('$5'),
                                                                                                                                                      packageItemList@ => '$6',
                                                                                                                                                      packageNameEnd@ => unwrap_2_list('$8'),
                                                                                                                                                      packageNameStart@ => unwrap_2_list('$1'),
                                                                                                                                                      plsqlPackageSourceAttributeList@ => '$3',
                                                                                                                                                      sharingClause@ => '$2'}}.
plsqlPackageSource -> NAME '.' NAME                                               as_is                          END      : #{plsqlPackageSource => #{asIs@ => '$4',
                                                                                                                                                      packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')])}}.
plsqlPackageSource -> NAME '.' NAME                                               as_is                          END NAME : #{plsqlPackageSource => #{asIs@ => '$4',
                                                                                                                                                      packageNameEnd@ => unwrap_2_list('$6'),
                                                                                                                                                      packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')])}}.
plsqlPackageSource -> NAME '.' NAME                                               as_is          packageItemList END      : #{plsqlPackageSource => #{asIs@ => '$4',
                                                                                                                                                      packageItemList@ => '$5',
                                                                                                                                                      packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')])}}.
plsqlPackageSource -> NAME '.' NAME                                               as_is          packageItemList END NAME : #{plsqlPackageSource => #{asIs@ => '$4',
                                                                                                                                                      packageItemList@ => '$5',
                                                                                                                                                      packageNameEnd@ => unwrap_2_list('$7'),
                                                                                                                                                      packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')])}}.
plsqlPackageSource -> NAME '.' NAME                                               as_is MAN_PAGE                 END      : #{plsqlPackageSource => #{asIs@ => '$4',
                                                                                                                                                      man_page@ => unwrap_2_list('$5'),
                                                                                                                                                      packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')])}}.
plsqlPackageSource -> NAME '.' NAME                                               as_is MAN_PAGE                 END NAME : #{plsqlPackageSource => #{asIs@ => '$4',
                                                                                                                                                      man_page@ => unwrap_2_list('$5'),
                                                                                                                                                      packageNameEnd@ => unwrap_2_list('$7'),
                                                                                                                                                      packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')])}}.
plsqlPackageSource -> NAME '.' NAME                                               as_is MAN_PAGE packageItemList END      : #{plsqlPackageSource => #{asIs@ => '$4',
                                                                                                                                                      man_page@ => unwrap_2_list('$5'),
                                                                                                                                                      packageItemList@ => '$6',
                                                                                                                                                      packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')])}}.
plsqlPackageSource -> NAME '.' NAME                                               as_is MAN_PAGE packageItemList END NAME : #{plsqlPackageSource => #{asIs@ => '$4',
                                                                                                                                                      man_page@ => unwrap_2_list('$5'),
                                                                                                                                                      packageItemList@ => '$6',
                                                                                                                                                      packageNameEnd@ => unwrap_2_list('$8'),
                                                                                                                                                      packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')])}}.
plsqlPackageSource -> NAME '.' NAME               plsqlPackageSourceAttributeList as_is                          END      : #{plsqlPackageSource => #{asIs@ => '$5',
                                                                                                                                                      packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')]),
                                                                                                                                                      plsqlPackageSourceAttributeList@ => '$4'}}.
plsqlPackageSource -> NAME '.' NAME               plsqlPackageSourceAttributeList as_is                          END NAME : #{plsqlPackageSource => #{asIs@ => '$5',
                                                                                                                                                      packageNameEnd@ => unwrap_2_list('$7'),
                                                                                                                                                      packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')]),
                                                                                                                                                      plsqlPackageSourceAttributeList@ => '$4'}}.
plsqlPackageSource -> NAME '.' NAME               plsqlPackageSourceAttributeList as_is          packageItemList END      : #{plsqlPackageSource => #{asIs@ => '$5',
                                                                                                                                                      packageItemList@ => '$6',
                                                                                                                                                      packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')]),
                                                                                                                                                      plsqlPackageSourceAttributeList@ => '$4'}}.
plsqlPackageSource -> NAME '.' NAME               plsqlPackageSourceAttributeList as_is          packageItemList END NAME : #{plsqlPackageSource => #{asIs@ => '$5',
                                                                                                                                                      packageItemList@ => '$6',
                                                                                                                                                      packageNameEnd@ => unwrap_2_list('$8'),
                                                                                                                                                      packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')]),
                                                                                                                                                      plsqlPackageSourceAttributeList@ => '$4'}}.
plsqlPackageSource -> NAME '.' NAME               plsqlPackageSourceAttributeList as_is MAN_PAGE                 END      : #{plsqlPackageSource => #{asIs@ => '$5',
                                                                                                                                                      man_page@ => unwrap_2_list('$6'),
                                                                                                                                                      packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')]),
                                                                                                                                                      plsqlPackageSourceAttributeList@ => '$4'}}.
plsqlPackageSource -> NAME '.' NAME               plsqlPackageSourceAttributeList as_is MAN_PAGE                 END NAME : #{plsqlPackageSource => #{asIs@ => '$5',
                                                                                                                                                      man_page@ => unwrap_2_list('$6'),
                                                                                                                                                      packageNameEnd@ => unwrap_2_list('$8'),
                                                                                                                                                      packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')]),
                                                                                                                                                      plsqlPackageSourceAttributeList@ => '$4'}}.
plsqlPackageSource -> NAME '.' NAME               plsqlPackageSourceAttributeList as_is MAN_PAGE packageItemList END      : #{plsqlPackageSource => #{asIs@ => '$5',
                                                                                                                                                      man_page@ => unwrap_2_list('$6'),
                                                                                                                                                      packageItemList@ => '$7',
                                                                                                                                                      packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')]),
                                                                                                                                                      plsqlPackageSourceAttributeList@ => '$4'}}.
plsqlPackageSource -> NAME '.' NAME               plsqlPackageSourceAttributeList as_is MAN_PAGE packageItemList END NAME : #{plsqlPackageSource => #{asIs@ => '$5',
                                                                                                                                                      man_page@ => unwrap_2_list('$6'),
                                                                                                                                                      packageItemList@ => '$7',
                                                                                                                                                      packageNameEnd@ => unwrap_2_list('$9'),
                                                                                                                                                      packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')]),
                                                                                                                                                      plsqlPackageSourceAttributeList@ => '$4'}}.
plsqlPackageSource -> NAME '.' NAME sharingClause                                 as_is                          END      : #{plsqlPackageSource => #{asIs@ => '$5',
                                                                                                                                                      packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')]),
                                                                                                                                                      sharingClause@ => '$4'}}.
plsqlPackageSource -> NAME '.' NAME sharingClause                                 as_is                          END NAME : #{plsqlPackageSource => #{asIs@ => '$5',
                                                                                                                                                      packageNameEnd@ => unwrap_2_list('$7'),
                                                                                                                                                      packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')]),
                                                                                                                                                      sharingClause@ => '$4'}}.
plsqlPackageSource -> NAME '.' NAME sharingClause                                 as_is          packageItemList END      : #{plsqlPackageSource => #{asIs@ => '$5',
                                                                                                                                                      packageItemList@ => '$6',
                                                                                                                                                      packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')]),
                                                                                                                                                      sharingClause@ => '$4'}}.
plsqlPackageSource -> NAME '.' NAME sharingClause                                 as_is          packageItemList END NAME : #{plsqlPackageSource => #{asIs@ => '$5',
                                                                                                                                                      packageItemList@ => '$6',
                                                                                                                                                      packageNameEnd@ => unwrap_2_list('$8'),
                                                                                                                                                      packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')]),
                                                                                                                                                      sharingClause@ => '$4'}}.
plsqlPackageSource -> NAME '.' NAME sharingClause                                 as_is MAN_PAGE                 END      : #{plsqlPackageSource => #{asIs@ => '$5',
                                                                                                                                                      man_page@ => unwrap_2_list('$6'),
                                                                                                                                                      packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')]),
                                                                                                                                                      sharingClause@ => '$4'}}.
plsqlPackageSource -> NAME '.' NAME sharingClause                                 as_is MAN_PAGE                 END NAME : #{plsqlPackageSource => #{asIs@ => '$5',
                                                                                                                                                      man_page@ => unwrap_2_list('$6'),
                                                                                                                                                      packageNameEnd@ => unwrap_2_list('$8'),
                                                                                                                                                      packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')]),
                                                                                                                                                      sharingClause@ => '$4'}}.
plsqlPackageSource -> NAME '.' NAME sharingClause                                 as_is MAN_PAGE packageItemList END      : #{plsqlPackageSource => #{asIs@ => '$5',
                                                                                                                                                      man_page@ => unwrap_2_list('$6'),
                                                                                                                                                      packageItemList@ => '$7',
                                                                                                                                                      packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')]),
                                                                                                                                                      sharingClause@ => '$4'}}.
plsqlPackageSource -> NAME '.' NAME sharingClause                                 as_is MAN_PAGE packageItemList END NAME : #{plsqlPackageSource => #{asIs@ => '$5',
                                                                                                                                                      man_page@ => unwrap_2_list('$6'),
                                                                                                                                                      packageItemList@ => '$7',
                                                                                                                                                      packageNameEnd@ => unwrap_2_list('$9'),
                                                                                                                                                      packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')]),
                                                                                                                                                      sharingClause@ => '$4'}}.
plsqlPackageSource -> NAME '.' NAME sharingClause plsqlPackageSourceAttributeList as_is                          END      : #{plsqlPackageSource => #{asIs@ => '$6',
                                                                                                                                                      packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')]),
                                                                                                                                                      plsqlPackageSourceAttributeList@ => '$5',
                                                                                                                                                      sharingClause@ => '$4'}}.
plsqlPackageSource -> NAME '.' NAME sharingClause plsqlPackageSourceAttributeList as_is                          END NAME : #{plsqlPackageSource => #{asIs@ => '$6',
                                                                                                                                                      packageNameEnd@ => unwrap_2_list('$8'),
                                                                                                                                                      packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')]),
                                                                                                                                                      plsqlPackageSourceAttributeList@ => '$5',
                                                                                                                                                      sharingClause@ => '$4'}}.
plsqlPackageSource -> NAME '.' NAME sharingClause plsqlPackageSourceAttributeList as_is          packageItemList END      : #{plsqlPackageSource => #{asIs@ => '$6',
                                                                                                                                                      packageItemList@ => '$7',
                                                                                                                                                      packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')]),
                                                                                                                                                      plsqlPackageSourceAttributeList@ => '$5',
                                                                                                                                                      sharingClause@ => '$4'}}.
plsqlPackageSource -> NAME '.' NAME sharingClause plsqlPackageSourceAttributeList as_is          packageItemList END NAME : #{plsqlPackageSource => #{asIs@ => '$6',
                                                                                                                                                      packageItemList@ => '$7',
                                                                                                                                                      packageNameEnd@ => unwrap_2_list('$9'),
                                                                                                                                                      packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')]),
                                                                                                                                                      plsqlPackageSourceAttributeList@ => '$5',
                                                                                                                                                      sharingClause@ => '$4'}}.
plsqlPackageSource -> NAME '.' NAME sharingClause plsqlPackageSourceAttributeList as_is MAN_PAGE                 END      : #{plsqlPackageSource => #{asIs@ => '$6',
                                                                                                                                                      man_page@ => unwrap_2_list('$7'),
                                                                                                                                                      packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')]),
                                                                                                                                                      plsqlPackageSourceAttributeList@ => '$5',
                                                                                                                                                      sharingClause@ => '$4'}}.
plsqlPackageSource -> NAME '.' NAME sharingClause plsqlPackageSourceAttributeList as_is MAN_PAGE                 END NAME : #{plsqlPackageSource => #{asIs@ => '$6',
                                                                                                                                                      man_page@ => unwrap_2_list('$7'),
                                                                                                                                                      packageNameEnd@ => unwrap_2_list('$9'),
                                                                                                                                                      packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')]),
                                                                                                                                                      plsqlPackageSourceAttributeList@ => '$5',
                                                                                                                                                      sharingClause@ => '$4'}}.
plsqlPackageSource -> NAME '.' NAME sharingClause plsqlPackageSourceAttributeList as_is MAN_PAGE packageItemList END      : #{plsqlPackageSource => #{asIs@ => '$6',
                                                                                                                                                      man_page@ => unwrap_2_list('$7'),
                                                                                                                                                      packageItemList@ => '$8',
                                                                                                                                                      packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')]),
                                                                                                                                                      plsqlPackageSourceAttributeList@ => '$5',
                                                                                                                                                      sharingClause@ => '$4'}}.
plsqlPackageSource -> NAME '.' NAME sharingClause plsqlPackageSourceAttributeList as_is MAN_PAGE packageItemList END NAME : #{plsqlPackageSource => #{asIs@ => '$6',
                                                                                                                                                      man_page@ => unwrap_2_list('$7'),
                                                                                                                                                      packageItemList@ => '$8',
                                                                                                                                                      packageNameEnd@ => unwrap_2_list('$10'),
                                                                                                                                                      packageNameStart@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')]),
                                                                                                                                                      plsqlPackageSourceAttributeList@ => '$5',
                                                                                                                                                      sharingClause@ => '$4'}}.

privilegeAnnotationList -> apiGroupAnnotation                                : ['$1'].
privilegeAnnotationList -> apiGroupAnnotation        privilegeAnnotationList : ['$1' | '$2'].
privilegeAnnotationList -> objectPrivilegeAnnotation                         : ['$1'].
privilegeAnnotationList -> objectPrivilegeAnnotation privilegeAnnotationList : ['$1' | '$2'].
privilegeAnnotationList -> systemPrivilegeAnnotation                         : ['$1'].
privilegeAnnotationList -> systemPrivilegeAnnotation privilegeAnnotationList : ['$1' | '$2'].

%% Level 05 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

apiGroupAnnotation -> '--<>' API_GROUP '=' NAME : #{apiGroupAnnotation => #{apiGroup@ => unwrap_2_list('$4'),
                                                                            type@ => unwrap_2_list('$2')}}.

as_is -> AS : unwrap_2_list('$1').
as_is -> IS : unwrap_2_list('$1').

objectPrivilegeAnnotation -> '--<>' OBJECT_PRIVILEGE objectPrivilegeType '=' NAME          : #{objectPrivilegeAnnotation => #{type@ => unwrap_2_list('$2'),
                                                                                                                              object@ => unwrap_2_list('$5'),
                                                                                                                              privilegeType@ => '$3'}}.
objectPrivilegeAnnotation -> '--<>' OBJECT_PRIVILEGE objectPrivilegeType '=' NAME '.' NAME : #{objectPrivilegeAnnotation => #{type@ => unwrap_2_list('$2'),
                                                                                                                              object@ => lists:append([unwrap_2_list('$5'), ".", unwrap_2_list('$7')]),
                                                                                                                              privilegeType@ => '$3'}}.

systemPrivilegeAnnotation -> '--<>' SYSTEM_PRIVILEGE '=' systemPrivilegeType : #{systemPrivilegeAnnotation => #{type@ => unwrap_2_list('$2'),
                                                                                                                privilegeType@ => '$4'}}.

%% Level 06 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

objectPrivilegeType -> INDEX               : unwrap_2_list('$1').
objectPrivilegeType -> NAME                : unwrap_2_list('$1').
objectPrivilegeType -> NAME NAME           : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2')]).
objectPrivilegeType -> NAME NAME   NAME    : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2'), " ", unwrap_2_list('$3')]).
objectPrivilegeType -> ON   COMMIT REFRESH : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2'), " ", unwrap_2_list('$3')]).

packageItem -> itemDeclaration             : '$1'.
packageItem -> packageFunctionDeclaration  : '$1'.
packageItem -> packageProcedureDeclaration : '$1'.
packageItem -> typeDefinition              : '$1'.

packageItemList -> packageItemSimple                      : ['$1'].
packageItemList -> packageItemConditional                 : ['$1'].
packageItemList -> packageItemSimple      packageItemList : ['$1' | '$2'].
packageItemList -> packageItemConditional packageItemList : ['$1' | '$2'].

plsqlPackageSourceAttributeList -> plsqlPackageSourceAttribute                                 : ['$1'].
plsqlPackageSourceAttributeList -> plsqlPackageSourceAttribute plsqlPackageSourceAttributeList : ['$1' | '$2'].

sharingClause -> SHARING '=' METADATA : #{sharingClause => unwrap_2_list('$3')}.
sharingClause -> SHARING '=' NONE     : #{sharingClause => unwrap_2_list('$3')}.

systemPrivilegeType -> CREATE ANY       NAME      : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2'), " ", unwrap_2_list('$3')]).
systemPrivilegeType -> CREATE NAME                : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2')]).
systemPrivilegeType -> CREATE NAME      NAME      : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2'), " ", unwrap_2_list('$3')]).
systemPrivilegeType -> CREATE PROCEDURE           : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2')]).
systemPrivilegeType -> CREATE TABLE               : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2')]).
systemPrivilegeType -> CREATE TRIGGER             : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2')]).
systemPrivilegeType -> CREATE TYPE                : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2')]).
systemPrivilegeType -> NAME                       : unwrap_2_list('$1').
systemPrivilegeType -> NAME   ANY       NAME      : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2'), " ", unwrap_2_list('$3')]).
systemPrivilegeType -> NAME   ANY       NAME NAME : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2'), " ", unwrap_2_list('$3'), " ", unwrap_2_list('$4')]).
systemPrivilegeType -> NAME   ANY       PROCEDURE : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2'), " ", unwrap_2_list('$3')]).
systemPrivilegeType -> NAME   ANY       TABLE     : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2'), " ", unwrap_2_list('$3')]).
systemPrivilegeType -> NAME   ANY       TRIGGER   : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2'), " ", unwrap_2_list('$3')]).
systemPrivilegeType -> NAME   ANY       TYPE      : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2'), " ", unwrap_2_list('$3')]).
systemPrivilegeType -> NAME   NAME                : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2')]).
systemPrivilegeType -> NAME   NAME      NAME      : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2'), " ", unwrap_2_list('$3')]).
systemPrivilegeType -> NAME   NAME      NAME NAME : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2'), " ", unwrap_2_list('$3'), " ", unwrap_2_list('$4')]).
systemPrivilegeType -> SET    CONTAINER           : lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2')]).

%% Level 07 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

accessibleByClause -> ACCESSIBLE BY '(' accessorCommaList ')' : #{accessibleByClause => #{accessorCommaList@ => '$4'}}.

defaultCollationClause -> DEFAULT COLLATION USING_NLS_COMP : #{defaultCollationClause => lists:append([unwrap_2_list('$1'), " ", unwrap_2_list('$2'), " ", unwrap_2_list('$3')])}.

invokerRightsClause -> AUTHID CURRENT_USER : #{invokerRightsClause => unwrap_2_list('$2')}.
invokerRightsClause -> AUTHID DEFINER      : #{invokerRightsClause => unwrap_2_list('$2')}.

itemDeclaration -> constantDeclaration  : '$1'.
itemDeclaration -> exceptionDeclaration : '$1'.
itemDeclaration -> pragmaDeclaration    : '$1'.
itemDeclaration -> variableDeclaration  : '$1'.

packageFunctionDeclaration ->                                        functionHeading                                                  ';' : #{type => "Function",
                                                                                                                                              packageFunctionDeclaration => #{functionHeading@ => '$1'}}.
packageFunctionDeclaration ->                                        functionHeading                                         MAN_PAGE ';' : #{type => "Function",
                                                                                                                                              packageFunctionDeclaration => #{functionHeading@ => '$1',
                                                                                                                                                                              man_page@ => unwrap_2_list('$2')}}.
packageFunctionDeclaration ->                                        functionHeading packageFunctionDeclarationAttributeList          ';' : #{type => "Function",
                                                                                                                                              packageFunctionDeclaration => #{functionHeading@ => '$1',
                                                                                                                                                                              packageFunctionDeclarationAttributeList@ => '$2'}}.
packageFunctionDeclaration ->                                        functionHeading packageFunctionDeclarationAttributeList MAN_PAGE ';' : #{type => "Function",
                                                                                                                                              packageFunctionDeclaration => #{functionHeading@ => '$1',
                                                                                                                                                                              man_page@ => unwrap_2_list('$3'),
                                                                                                                                                                              packageFunctionDeclarationAttributeList@ => '$2'}}.
packageFunctionDeclaration ->                     functionAnnotation functionHeading                                                  ';' : #{type => "Function",
                                                                                                                                              packageFunctionDeclaration => #{functionAnnotation@ => '$1',
                                                                                                                                                                              functionHeading@ => '$2'}}.
packageFunctionDeclaration ->                     functionAnnotation functionHeading                                         MAN_PAGE ';' : #{type => "Function",
                                                                                                                                              packageFunctionDeclaration => #{functionAnnotation@ => '$1',
                                                                                                                                                                              man_page@ => unwrap_2_list('$3'),
                                                                                                                                                                              functionHeading@ => '$2'}}.
packageFunctionDeclaration ->                     functionAnnotation functionHeading packageFunctionDeclarationAttributeList          ';' : #{type => "Function",                                                                                                                                                                                                             packageFunctionDeclaration => #{functionAnnotation@ => '$1',
                                                                                                                                                                              functionHeading@ => '$2',
                                                                                                                                                                              packageFunctionDeclarationAttributeList@ => '$3'}}.
packageFunctionDeclaration ->                     functionAnnotation functionHeading packageFunctionDeclarationAttributeList MAN_PAGE ';' : #{type => "Function",
                                                                                                                                              packageFunctionDeclaration => #{functionAnnotation@ => '$1',
                                                                                                                                                                              functionHeading@ => '$2',
                                                                                                                                                                              man_page@ => unwrap_2_list('$4'),
                                                                                                                                                                              packageFunctionDeclarationAttributeList@ => '$3'}}.
packageFunctionDeclaration -> apiHiddenAnnotation                    functionHeading                                                  ';' : #{type => "Function",
                                                                                                                                              packageFunctionDeclaration => #{apiHiddenAnnotation@ => '$1',
                                                                                                                                                                              functionHeading@ => '$2'}}.
packageFunctionDeclaration -> apiHiddenAnnotation                    functionHeading                                         MAN_PAGE ';' : #{type => "Function",
                                                                                                                                              packageFunctionDeclaration => #{apiHiddenAnnotation@ => '$1',
                                                                                                                                                                              man_page@ => unwrap_2_list('$3'),
                                                                                                                                                                              functionHeading@ => '$2'}}.
packageFunctionDeclaration -> apiHiddenAnnotation                    functionHeading packageFunctionDeclarationAttributeList          ';' : #{type => "Function",
                                                                                                                                              packageFunctionDeclaration => #{apiHiddenAnnotation@ => '$1',
                                                                                                                                                                              functionHeading@ => '$2',
                                                                                                                                                                              packageFunctionDeclarationAttributeList@ => '$3'}}.
packageFunctionDeclaration -> apiHiddenAnnotation                    functionHeading packageFunctionDeclarationAttributeList MAN_PAGE ';' : #{type => "Function",
                                                                                                                                              packageFunctionDeclaration => #{apiHiddenAnnotation@ => '$1',
                                                                                                                                                                              functionHeading@ => '$2',
                                                                                                                                                                              man_page@ => unwrap_2_list('$4'),
                                                                                                                                                                              packageFunctionDeclarationAttributeList@ => '$3'}}.
packageFunctionDeclaration -> apiHiddenAnnotation functionAnnotation functionHeading                                                  ';' : #{type => "Function",
                                                                                                                                              packageFunctionDeclaration => #{apiHiddenAnnotation@ => '$1',
                                                                                                                                                                              functionAnnotation@ => '$2',
                                                                                                                                                                              functionHeading@ => '$3'}}.
packageFunctionDeclaration -> apiHiddenAnnotation functionAnnotation functionHeading                                         MAN_PAGE ';' : #{type => "Function",
                                                                                                                                              packageFunctionDeclaration => #{apiHiddenAnnotation@ => '$1',
                                                                                                                                                                              functionAnnotation@ => '$2',
                                                                                                                                                                              functionHeading@ => '$3',
                                                                                                                                                                              man_page@ => unwrap_2_list('$4')}}.
packageFunctionDeclaration -> apiHiddenAnnotation functionAnnotation functionHeading packageFunctionDeclarationAttributeList          ';' : #{type => "Function",
                                                                                                                                              packageFunctionDeclaration => #{apiHiddenAnnotation@ => '$1',
                                                                                                                                                                              functionAnnotation@ => '$2',
                                                                                                                                                                              functionHeading@ => '$3',
                                                                                                                                                                              packageFunctionDeclarationAttributeList@ => '$4'}}.
packageFunctionDeclaration -> apiHiddenAnnotation functionAnnotation functionHeading packageFunctionDeclarationAttributeList MAN_PAGE ';' : #{type => "Function",
                                                                                                                                              packageFunctionDeclaration => #{apiHiddenAnnotation@ => '$1',
                                                                                                                                                                              functionAnnotation@ => '$2',
                                                                                                                                                                              functionHeading@ => '$3',
                                                                                                                                                                              man_page@ => unwrap_2_list('$5'),
                                                                                                                                                                              packageFunctionDeclarationAttributeList@ => '$4'}}.

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

packageProcedureDeclaration ->                                         procedureHeading                             ';' : #{type => "Procedure",
                                                                                                                            packageProcedureDeclaration => #{procedureHeading@ => '$1'}}.
packageProcedureDeclaration ->                                         procedureHeading                    MAN_PAGE ';' : #{type => "Procedure",
                                                                                                                            packageProcedureDeclaration => #{man_page@ => unwrap_2_list('$2'),
                                                                                                                                                             procedureHeading@ => '$1'}}.
packageProcedureDeclaration ->                                         procedureHeading accessibleByClause          ';' : #{type => "Procedure",
                                                                                                                            packageProcedureDeclaration => #{accessibleByClause@ => '$2',
                                                                                                                                                             procedureHeading@ => '$1'}}.
packageProcedureDeclaration ->                                         procedureHeading accessibleByClause MAN_PAGE ';' : #{type => "Procedure",
                                                                                                                            packageProcedureDeclaration => #{accessibleByClause@ => '$2',
                                                                                                                                                             man_page@ => unwrap_2_list('$3'),
                                                                                                                                                             procedureHeading@ => '$1'}}.
packageProcedureDeclaration ->                     procedureAnnotation procedureHeading                             ';' : #{type => "Procedure",
                                                                                                                            packageProcedureDeclaration => #{procedureAnnotation@ => '$1',
                                                                                                                                                             procedureHeading@ => '$2'}}.
packageProcedureDeclaration ->                     procedureAnnotation procedureHeading                    MAN_PAGE ';' : #{type => "Procedure",
                                                                                                                            packageProcedureDeclaration => #{man_page@ => unwrap_2_list('$3'),
                                                                                                                                                             procedureAnnotation@ => '$1',
                                                                                                                                                             procedureHeading@ => '$2'}}.
packageProcedureDeclaration ->                     procedureAnnotation procedureHeading accessibleByClause          ';' : #{type => "Procedure",
                                                                                                                            packageProcedureDeclaration => #{accessibleByClause@ => '$3',
                                                                                                                                                             procedureAnnotation@ => '$1',
                                                                                                                                                             procedureHeading@ => '$2'}}.
packageProcedureDeclaration ->                     procedureAnnotation procedureHeading accessibleByClause MAN_PAGE ';' : #{type => "Procedure",
                                                                                                                            packageProcedureDeclaration => #{accessibleByClause@ => '$3',
                                                                                                                                                             man_page@ => unwrap_2_list('$4'),
                                                                                                                                                             procedureAnnotation@ => '$1',
                                                                                                                                                             procedureHeading@ => '$2'}}.
packageProcedureDeclaration -> apiHiddenAnnotation                     procedureHeading                             ';' : #{type => "Procedure",
                                                                                                                            packageProcedureDeclaration => #{apiHiddenAnnotation@ => '$1',
                                                                                                                                                             procedureHeading@ => '$2'}}.
packageProcedureDeclaration -> apiHiddenAnnotation                     procedureHeading                    MAN_PAGE ';' : #{type => "Procedure",
                                                                                                                            packageProcedureDeclaration => #{apiHiddenAnnotation@ => '$1',
                                                                                                                                                             man_page@ => unwrap_2_list('$3'),
                                                                                                                                                             procedureHeading@ => '$2'}}.
packageProcedureDeclaration -> apiHiddenAnnotation                     procedureHeading accessibleByClause          ';' : #{type => "Procedure",
                                                                                                                            packageProcedureDeclaration => #{apiHiddenAnnotation@ => '$1',
                                                                                                                                                             procedureHeading@ => '$2',
                                                                                                                                                             accessibleByClause@ => '$3'}}.
packageProcedureDeclaration -> apiHiddenAnnotation                     procedureHeading accessibleByClause MAN_PAGE ';' : #{type => "Procedure",
                                                                                                                            packageProcedureDeclaration => #{apiHiddenAnnotation@ => '$1',
                                                                                                                                                             man_page@ => unwrap_2_list('$4'),
                                                                                                                                                             procedureHeading@ => '$2',
                                                                                                                                                             accessibleByClause@ => '$3'}}.
packageProcedureDeclaration -> apiHiddenAnnotation procedureAnnotation procedureHeading                             ';' : #{type => "Procedure",
                                                                                                                            packageProcedureDeclaration => #{apiHiddenAnnotation@ => '$1',
                                                                                                                                                             procedureAnnotation@ => '$2',
                                                                                                                                                             procedureHeading@ => '$3'}}.
packageProcedureDeclaration -> apiHiddenAnnotation procedureAnnotation procedureHeading                    MAN_PAGE ';' : #{type => "Procedure",
                                                                                                                            packageProcedureDeclaration => #{apiHiddenAnnotation@ => '$1',
                                                                                                                                                             man_page@ => unwrap_2_list('$4'),
                                                                                                                                                             procedureAnnotation@ => '$2',
                                                                                                                                                             procedureHeading@ => '$3'}}.
packageProcedureDeclaration -> apiHiddenAnnotation procedureAnnotation procedureHeading accessibleByClause          ';' : #{type => "Procedure",
                                                                                                                            packageProcedureDeclaration => #{apiHiddenAnnotation@ => '$1',
                                                                                                                                                             procedureAnnotation@ => '$2',
                                                                                                                                                             procedureHeading@ => '$3',
                                                                                                                                                             accessibleByClause@ => '$4'}}.
packageProcedureDeclaration -> apiHiddenAnnotation procedureAnnotation procedureHeading accessibleByClause MAN_PAGE ';' : #{type => "Procedure",
                                                                                                                            packageProcedureDeclaration => #{apiHiddenAnnotation@ => '$1',
                                                                                                                                                             man_page@ => unwrap_2_list('$5'),
                                                                                                                                                             procedureAnnotation@ => '$2',
                                                                                                                                                             procedureHeading@ => '$3',
                                                                                                                                                             accessibleByClause@ => '$4'}}.
typeDefinition -> collectionTypeDefinition : '$1'.
typeDefinition -> recordTypeDefinition     : '$1'.
typeDefinition -> refCursorTypeDefinition  : '$1'.
typeDefinition -> subtypeDefinition        : '$1'.

%% Level 08 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

accessorCommaList -> accessor                       : ['$1'].
accessorCommaList -> accessor ',' accessorCommaList : ['$1' | '$3'].

collectionTypeDefinition -> TYPE NAME IS assocArrayTypeDef ';' : #{type => "CollectionType",
                                                                   collectionTypeDefinition => #{typeName@ => #{typeName => unwrap_2_list('$2')},
                                                                                                 assocArrayTypeDef@ => '$4'}}.
collectionTypeDefinition -> TYPE NAME IS varrayTypeDef     ';' : #{type => "CollectionType",
                                                                   collectionTypeDefinition => #{typeName@ => #{typeName => unwrap_2_list('$2')},
                                                                                                 varrayTypeDef@ => '$4'}}.

constantDeclaration -> NAME CONSTANT dataType_1           default ';' : #{type => "Constant",
                                                                          constantDeclaration => #{constantName@ => #{constantName => unwrap_2_list('$1')},
                                                                                                   dataType@ => '$3',
                                                                                                   default@ => '$4'}}.
constantDeclaration -> NAME CONSTANT dataType_1 NOT NULLX default ';' : #{type => "Constant",
                                                                          constantDeclaration => #{constantName@ => #{constantName => unwrap_2_list('$1')},
                                                                                                   dataType@ => '$3',
                                                                                                   notNull@ => #{notNull => "not null"},
                                                                                                   default@ => '$6'}}.
constantDeclaration -> NAME CONSTANT dataType_2           default ';' : #{type => "Constant",
                                                                          constantDeclaration => #{constantName@ => #{constantName => unwrap_2_list('$1')},
                                                                                                   dataType@ => '$3',
                                                                                                   default@ => '$4'}}.
constantDeclaration -> NAME CONSTANT dataType_2 NOT NULLX default ';' : #{type => "Constant",
                                                                          constantDeclaration => #{constantName@ => #{constantName => unwrap_2_list('$1')},
                                                                                                   dataType@ => '$3',
                                                                                                   notNull@ => #{notNull => "not null"},
                                                                                                   default@ => '$6'}}.

exceptionDeclaration -> NAME EXCEPTION ';' : #{type => "Exception",
                                               exceptionDeclaration => unwrap_2_list('$1')}.

fieldDefinitionCommaList -> fieldDefinition                              : ['$1'].
fieldDefinitionCommaList -> fieldDefinition ',' fieldDefinitionCommaList : ['$1' | '$3'].

functionAnnotation -> privilegeAnnotationList : #{functionAnnotation => #{privilegeAnnotationList@ => '$1'}}.

functionHeading -> FUNCTION nameExtended                                       RETURN dataType_1 : #{functionHeading => #{name@ => '$2',
                                                                                                                          return@ => '$4'}}.
functionHeading -> FUNCTION nameExtended                                       RETURN dataType_2 : #{functionHeading => #{name@ => '$2',
                                                                                                                          return@ => '$4'}}.
functionHeading -> FUNCTION nameExtended '(' parameterDeclarationCommaList ')' RETURN dataType_1 : #{functionHeading => #{name@ => '$2',
                                                                                                                          parameterDeclarationCommaList@ => '$4',
                                                                                                                          return@ => '$7'}}.
functionHeading -> FUNCTION nameExtended '(' parameterDeclarationCommaList ')' RETURN dataType_2 : #{functionHeading => #{name@ => '$2',
                                                                                                                          parameterDeclarationCommaList@ => '$4',
                                                                                                                          return@ => '$7'}}.

packageFunctionDeclarationAttributeList -> packageFunctionDeclarationAttribute                                         : ['$1'].
packageFunctionDeclarationAttributeList -> packageFunctionDeclarationAttribute packageFunctionDeclarationAttributeList : ['$1' | '$2'].

procedureAnnotation -> privilegeAnnotationList : #{procedureAnnotation => #{privilegeAnnotationList@ => '$1'}}.

procedureHeading -> PROCEDURE NAME                                       : #{procedureHeading => #{name@ => unwrap_2_list('$2')}}.
procedureHeading -> PROCEDURE NAME '(' parameterDeclarationCommaList ')' : #{procedureHeading => #{name@ => unwrap_2_list('$2'),
                                                                                                   parameterDeclarationCommaList@ => '$4'}}.

recordTypeDefinition -> TYPE NAME IS RECORD '(' fieldDefinitionCommaList ')' ';' : #{type => "RecordType",
                                                                                     recordTypeDefinition => #{recordTypeName@ => #{recordTypeName => unwrap_2_list('$2')},
                                                                                                               fieldDefinitionCommaList@ => '$6'}}.

refCursorTypeDefinition -> TYPE NAME IS REF CURSOR RETURN NAME                              ';' : #{type => "RefCursorType",
                                                                                                    refCursorTypeDefinition => #{name@ => unwrap_2_list('$2'),
                                                                                                    type@ => unwrap_2_list('$7')}}.
refCursorTypeDefinition -> TYPE NAME IS REF CURSOR RETURN NAME                   '%ROWTYPE' ';' : #{type => "RefCursorType",
                                                                                                    refCursorTypeDefinition => #{name@ => unwrap_2_list('$2'),
                                                                                                    type@ => unwrap_2_list('$7'),
                                                                                                    attribute@ => unwrap_2_list('$8')}}.
refCursorTypeDefinition -> TYPE NAME IS REF CURSOR RETURN NAME                   '%TYPE'    ';' : #{type => "RefCursorType",
                                                                                                    refCursorTypeDefinition => #{name@ => unwrap_2_list('$2'),
                                                                                                    type@ => unwrap_2_list('$7'),
                                                                                                    attribute@ => unwrap_2_list('$8')}}.
refCursorTypeDefinition -> TYPE NAME IS REF CURSOR RETURN NAME '.' NAME          '%ROWTYPE' ';' : #{type => "RefCursorType",
                                                                                                    refCursorTypeDefinition => #{name@ => unwrap_2_list('$2'),
                                                                                                    type@ => lists:append([unwrap_2_list('$7'), ".", unwrap_2_list('$9')]),
                                                                                                    attribute@ => unwrap_2_list('$10')}}.
refCursorTypeDefinition -> TYPE NAME IS REF CURSOR RETURN NAME '.' NAME          '%TYPE'    ';' : #{type => "RefCursorType",
                                                                                                    refCursorTypeDefinition => #{name@ => unwrap_2_list('$2'),
                                                                                                    type@ => lists:append([unwrap_2_list('$7'), ".", unwrap_2_list('$9')]),
                                                                                                    attribute@ => unwrap_2_list('$10')}}.
refCursorTypeDefinition -> TYPE NAME IS REF CURSOR RETURN NAME '.' NAME '.' NAME '%TYPE'    ';' : #{type => "RefCursorType",
                                                                                                    refCursorTypeDefinition => #{name@ => unwrap_2_list('$2'),
                                                                                                    type@ => lists:append([unwrap_2_list('$7'), ".", unwrap_2_list('$9'), ".", unwrap_2_list('$11')]),
                                                                                                    attribute@ => unwrap_2_list('$12')}}.

subtypeDefinition -> SUBTYPE NAME IS dataType_1           ';' : #{type => "Subtype",
                                                                  subtypeDefinition => #{subtypeName@ => #{subtypeName => unwrap_2_list('$2')},
                                                                                         dataType@ => '$4'}}.
subtypeDefinition -> SUBTYPE NAME IS dataType_2           ';' : #{type => "Subtype",
                                                                  subtypeDefinition => #{subtypeName@ => #{subtypeName => unwrap_2_list('$2')},
                                                                                         dataType@ => '$4'}}.
subtypeDefinition -> SUBTYPE NAME IS dataType_1 NOT NULLX ';' : #{type => "Subtype",
                                                                  subtypeDefinition => #{subtypeName@ => #{subtypeName => unwrap_2_list('$2')},
                                                                                         dataType@ => '$4',
                                                                                         notNull@ => #{notNull => "not null"}}}.
subtypeDefinition -> SUBTYPE NAME IS dataType_2 NOT NULLX ';' : #{type => "Subtype",
                                                                  subtypeDefinition => #{subtypeName@ => #{subtypeName => unwrap_2_list('$2')},
                                                                                         dataType@ => '$4',
                                                                                         notNull@ => #{notNull => "not null"}}}.

pragmaDeclaration -> PRAGMA EXCEPTION_INIT pragmaParameterExceptionInit           ';' : #{type => "Pragma",
                                                                                          pragmaDeclaration => #{pragmaParameter@ => '$3',
                                                                                                                 pragmaType@ => "EXCEPTION_INIT"}}.
pragmaDeclaration -> PRAGMA RESTRICT_REFERENCES pragmaParameterRestrictReferences ';' : #{type => "Pragma",
                                                                                          pragmaDeclaration => #{pragmaParameter@ => '$3',
                                                                                                                 pragmaType@ => "RESTRICT_REFERENCES"}}.
pragmaDeclaration -> PRAGMA SERIALLY_REUSABLE                                     ';' : #{type => "Pragma",
                                                                                          pragmaDeclaration => #{pragmaType@ => "SERIALLY_REUSABLE"}}.
pragmaDeclaration -> PRAGMA UDF                                                   ';' : #{type => "Pragma",
                                                                                          pragmaDeclaration => #{pragmaType@ => "UDF"}}.

pragmaParameterExceptionInit -> '(' NAME ','     INTNUM ')' : #{pragmaParameter => #{name@ => unwrap_2_list('$2'),
                                                                                     value@ => unwrap_2_list('$4')}}.
pragmaParameterExceptionInit -> '(' NAME ',' '-' INTNUM ')' : #{pragmaParameter => #{name@ => unwrap_2_list('$2'),
                                                                                     sign@ => unwrap_2_list('$4'),
                                                                                     value@ => unwrap_2_list('$5')}}.

pragmaParameterRestrictReferences -> '(' DEFAULT ',' restrictReferencesList ')' : #{pragmaParameter => #{name@ => "DEFAULT",
                                                                                                         restrictReferencesList@ => '$4'}}.
pragmaParameterRestrictReferences -> '(' NAME    ',' restrictReferencesList ')' : #{pragmaParameter => #{name@ => unwrap_2_list('$2'),
                                                                                                         restrictReferencesList@ => '$4'}}.

restrictReferencesList -> RNDS                             : [#{restrictReferences => unwrap_2_list('$1') ++ " "}].
restrictReferencesList -> RNDS      restrictReferencesList : [#{restrictReferences => unwrap_2_list('$1') ++ " "} | '$2'].
restrictReferencesList -> RNDS  ','                        : [#{restrictReferences => unwrap_2_list('$1') ++ ","}].
restrictReferencesList -> RNDS  ',' restrictReferencesList : [#{restrictReferences => unwrap_2_list('$1') ++ ","} | '$3'].
restrictReferencesList -> RNPS                             : [#{restrictReferences => unwrap_2_list('$1') ++ " "}].
restrictReferencesList -> RNPS      restrictReferencesList : [#{restrictReferences => unwrap_2_list('$1') ++ " "} | '$2'].
restrictReferencesList -> RNPS  ','                        : [#{restrictReferences => unwrap_2_list('$1') ++ ","}].
restrictReferencesList -> RNPS  ',' restrictReferencesList : [#{restrictReferences => unwrap_2_list('$1') ++ ","} | '$3'].
restrictReferencesList -> TRUST                            : [#{restrictReferences => unwrap_2_list('$1') ++ " "}].
restrictReferencesList -> TRUST     restrictReferencesList : [#{restrictReferences => unwrap_2_list('$1') ++ " "} | '$2'].
restrictReferencesList -> TRUST ','                        : [#{restrictReferences => unwrap_2_list('$1') ++ ","}].
restrictReferencesList -> TRUST ',' restrictReferencesList : [#{restrictReferences => unwrap_2_list('$1') ++ ","} | '$3'].
restrictReferencesList -> WNDS                             : [#{restrictReferences => unwrap_2_list('$1') ++ " "}].
restrictReferencesList -> WNDS      restrictReferencesList : [#{restrictReferences => unwrap_2_list('$1') ++ " "} | '$2'].
restrictReferencesList -> WNDS  ','                        : [#{restrictReferences => unwrap_2_list('$1') ++ ","}].
restrictReferencesList -> WNDS  ',' restrictReferencesList : [#{restrictReferences => unwrap_2_list('$1') ++ ","} | '$3'].
restrictReferencesList -> WNPS                             : [#{restrictReferences => unwrap_2_list('$1') ++ " "}].
restrictReferencesList -> WNPS      restrictReferencesList : [#{restrictReferences => unwrap_2_list('$1') ++ " "} | '$2'].
restrictReferencesList -> WNPS  ','                        : [#{restrictReferences => unwrap_2_list('$1') ++ ","}].
restrictReferencesList -> WNPS  ',' restrictReferencesList : [#{restrictReferences => unwrap_2_list('$1') ++ ","} | '$3'].

variableDeclaration -> NAME dataType_1                   ';' : #{type => "Variable",
                                                                 variableDeclaration => #{name@ => unwrap_2_list('$1'),
                                                                                          dataType@ => '$2'}}.
variableDeclaration -> NAME dataType_1           default ';' : #{type => "Variable",
                                                                 variableDeclaration => #{name@ => unwrap_2_list('$1'),
                                                                                          dataType@ => '$2',
                                                                                          default@ => '$3'}}.
variableDeclaration -> NAME dataType_1 NOT NULLX         ';' : #{type => "Variable",
                                                                 variableDeclaration => #{name@ => unwrap_2_list('$1'),
                                                                                          dataType@ => '$2',
                                                                                          notNull@ => #{notNull => "not null"}}}.
variableDeclaration -> NAME dataType_1 NOT NULLX default ';' : #{type => "Variable",
                                                                 variableDeclaration => #{name@ => unwrap_2_list('$1'),
                                                                                          dataType@ => '$2',
                                                                                          notNull@ => #{notNull => "not null"},
                                                                                          default@ => '$5'}}.
variableDeclaration -> NAME dataType_2                   ';' : #{type => "Variable",
                                                                 variableDeclaration => #{name@ => unwrap_2_list('$1'),
                                                                                          dataType@ => '$2'}}.
variableDeclaration -> NAME dataType_2           default ';' : #{type => "Variable",
                                                                 variableDeclaration => #{name@ => unwrap_2_list('$1'),
                                                                                          dataType@ => '$2',
                                                                                          default@ => '$3'}}.
variableDeclaration -> NAME dataType_2 NOT NULLX         ';' : #{type => "Variable",
                                                                 variableDeclaration => #{name@ => unwrap_2_list('$1'),
                                                                                          dataType@ => '$2',
                                                                                          notNull@ => #{notNull => "not null"}}}.
variableDeclaration -> NAME dataType_2 NOT NULLX default ';' : #{type => "Variable",
                                                                 variableDeclaration => #{name@ => unwrap_2_list('$1'),
                                                                                          dataType@ => '$2',
                                                                                          notNull@ => #{notNull => "not null"},
                                                                                          default@ => '$5'}}.

%% Level 09 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

accessor ->                   NAME : #{accessor => #{name@ => unwrap_2_list('$1')}}.
accessor ->          NAME '.' NAME : #{accessor => #{name@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')])}}.
accessor -> unitKind          NAME : #{accessor => #{unitKind@ => '$1',
                                                     name@ => unwrap_2_list('$2')}}.
accessor -> unitKind NAME '.' NAME : #{accessor => #{unitKind@ => '$1',
                                                     name@ => lists:append([unwrap_2_list('$2'), ".", unwrap_2_list('$4')])}}.

apiHiddenAnnotation -> '--<>' API_HIDDEN '=' TRUE : #{apiHiddenAnnotation => #{apiHidden@ => unwrap_2_list('$4'),
                                                                               type@ => unwrap_2_list('$2')}}.

assocArrayTypeDef -> TABLE OF dataType_1                               : #{assocArrayTypeDef => #{dataTypeTable@ => #{dataTypeTable => "table of", dataType@ => '$3'}}}.
assocArrayTypeDef -> TABLE OF dataType_1           INDEX BY dataType_2 : #{assocArrayTypeDef => #{dataTypeTable@ => #{dataTypeTable => "table of", dataType@ => '$3'},
                                                                                                  dataTypeIndex@ => #{dataTypeIndex => "index by", dataType@ => '$6'}}}.
assocArrayTypeDef -> TABLE OF dataType_1           INDEX BY dataType_3 : #{assocArrayTypeDef => #{dataTypeTable@ => #{dataTypeTable => "table of", dataType@ => '$3'},
                                                                                                  dataTypeIndex@ => #{dataTypeIndex => "index by", dataType@ => '$6'}}}.
assocArrayTypeDef -> TABLE OF dataType_1 NOT NULLX                     : #{assocArrayTypeDef => #{dataTypeTable@ => #{dataTypeTable => "table of", dataType@ => '$3'},
                                                                                                  notNull@ => #{notNull => "not null"}}}.
assocArrayTypeDef -> TABLE OF dataType_1 NOT NULLX INDEX BY dataType_2 : #{assocArrayTypeDef => #{dataTypeTable@ => #{dataTypeTable => "table of", dataType@ => '$3'},
                                                                                                  notNull@ => #{notNull => "not null"},
                                                                                                  dataTypeIndex@ => #{dataTypeIndex => "index by", dataType@ => '$8'}}}.
assocArrayTypeDef -> TABLE OF dataType_1 NOT NULLX INDEX BY dataType_3 : #{assocArrayTypeDef => #{dataTypeTable@ => #{dataTypeTable => "table of", dataType@ => '$3'},
                                                                                                  notNull@ => #{notNull => "not null"},
                                                                                                  dataTypeIndex@ => #{dataTypeIndex => "index by", dataType@ => '$8'}}}.
assocArrayTypeDef -> TABLE OF dataType_2                               : #{assocArrayTypeDef => #{dataTypeTable@ => #{dataTypeTable => "table of", dataType@ => '$3'}}}.
assocArrayTypeDef -> TABLE OF dataType_2           INDEX BY dataType_2 : #{assocArrayTypeDef => #{dataTypeTable@ => #{dataTypeTable => "table of", dataType@ => '$3'},
                                                                                                  dataTypeIndex@ => #{dataTypeIndex => "index by", dataType@ => '$6'}}}.
assocArrayTypeDef -> TABLE OF dataType_2           INDEX BY dataType_3 : #{assocArrayTypeDef => #{dataTypeTable@ => #{dataTypeTable => "table of", dataType@ => '$3'},
                                                                                                  dataTypeIndex@ => #{dataTypeIndex => "index by", dataType@ => '$6'}}}.
assocArrayTypeDef -> TABLE OF dataType_2 NOT NULLX                     : #{assocArrayTypeDef => #{dataTypeTable@ => #{dataTypeTable => "table of", dataType@ => '$3'},
                                                                                                  notNull@ => #{notNull => "not null"}}}.
assocArrayTypeDef -> TABLE OF dataType_2 NOT NULLX INDEX BY dataType_2 : #{assocArrayTypeDef => #{dataTypeTable@ => #{dataTypeTable => "table of", dataType@ => '$3'},
                                                                                                  notNull@ => #{notNull => "not null"},
                                                                                                  dataTypeIndex@ => #{dataTypeIndex => "index by", dataType@ => '$8'}}}.
assocArrayTypeDef -> TABLE OF dataType_2 NOT NULLX INDEX BY dataType_3 : #{assocArrayTypeDef => #{dataTypeTable@ => #{dataTypeTable => "table of", dataType@ => '$3'},
                                                                                                  notNull@ => #{notNull => "not null"},
                                                                                                  dataTypeIndex@ => #{dataTypeIndex => "index by", dataType@ => '$8'}}}.

dataType_1 -> BFILE                                                            : #{dataType => #{class@ => sql,
                                                                                                 type@ => unwrap_2_list('$1')}}.
dataType_1 -> BINARY_DOUBLE                                                    : #{dataType => #{class@ => sql,
                                                                                                 type@ => unwrap_2_list('$1')}}.
dataType_1 -> BINARY_FLOAT                                                     : #{dataType => #{class@ => sql,
                                                                                                 type@ => unwrap_2_list('$1')}}.
dataType_1 -> BLOB                                                             : #{dataType => #{class@ => sql,
                                                                                                 type@ => unwrap_2_list('$1')}}.
dataType_1 -> BOOLEAN                                                          : #{dataType => #{class@ => plsql,
                                                                                                 type@ => unwrap_2_list('$1')}}.
dataType_1 -> CHAR                                                             : #{dataType => #{class@ => sql,
                                                                                                 type@ => unwrap_2_list('$1')}}.
dataType_1 -> CHAR          '(' INTNUM            ')'                          : #{dataType => #{class@ => sql,
                                                                                                 type@ => unwrap_2_list('$1'),
                                                                                                 size@ => unwrap_2_list('$3')}}.
dataType_1 -> CHAR          '(' INTNUM     BYTE   ')'                          : #{dataType => #{class@ => sql,
                                                                                                 type@ => unwrap_2_list('$1'),
                                                                                                 size@ => unwrap_2_list('$3'),
                                                                                                 sizeType@ => unwrap_2_list('$4')}}.
dataType_1 -> CHAR          '(' INTNUM     CHAR   ')'                          : #{dataType => #{class@ => sql,
                                                                                                 type@ => unwrap_2_list('$1'),
                                                                                                 size@ => unwrap_2_list('$3'),
                                                                                                 sizeType@ => unwrap_2_list('$4')}}.
dataType_1 -> CLOB                                                             : #{dataType => #{class@ => sql,
                                                                                                 type@ => unwrap_2_list('$1')}}.
dataType_1 -> DATE                                                             : #{dataType => #{class@ => sql,
                                                                                                 type@ => unwrap_2_list('$1')}}.
dataType_1 -> FLOAT                                                            : #{dataType => #{class@ => sql,
                                                                                                 type@ => unwrap_2_list('$1')}}.
dataType_1 -> FLOAT         '(' INTNUM            ')'                          : #{dataType => #{class@ => sql,
                                                                                                 type@ => unwrap_2_list('$1'),
                                                                                                 precision@ => unwrap_2_list('$3')}}.
dataType_1 -> INTERVAL DAY                            TO SECOND                : #{dataType => #{class@ => sql,
                                                                                                 type@ => "INTERVAL DAY"}}.
dataType_1 -> INTERVAL DAY  '(' INTNUM            ')' TO SECOND                : #{dataType => #{class@ => sql,
                                                                                                 type@ => "INTERVAL DAY",
                                                                                                 dayPrecision@ => unwrap_2_list('$4')}}.
dataType_1 -> INTERVAL DAY                            TO SECOND '(' INTNUM ')' : #{dataType => #{class@ => sql,
                                                                                                 type@ => "INTERVAL DAY",
                                                                                                 secondPrecision@ => unwrap_2_list('$6')}}.
dataType_1 -> INTERVAL DAY  '(' INTNUM            ')' TO SECOND '(' INTNUM ')' : #{dataType => #{class@ => sql,
                                                                                                 type@ => "INTERVAL DAY",
                                                                                                 dayPrecision@ => unwrap_2_list('$4'),
                                                                                                 secondPrecision@ => unwrap_2_list('$9')}}.
dataType_1 -> INTERVAL YEAR                           TO MONTH                 : #{dataType => #{class@ => sql,
                                                                                                 type@ => "INTERVAL YEAR"}}.
dataType_1 -> INTERVAL YEAR '(' INTNUM            ')' TO MONTH                 : #{dataType => #{class@ => sql,
                                                                                                 type@ => "INTERVAL YEAR",
                                                                                                 precision@ => unwrap_2_list('$4')}}.
dataType_1 -> LONG RAW                                                         : #{dataType => #{class@ => sql,
                                                                                                 type@ => "LONG RAW"}}.
dataType_1 -> NAME                                                             : #{dataType => #{class@ => user_defined,
                                                                                                 type@ => unwrap_2_list('$1')}}.
dataType_1 -> NAME '.' NAME                                                    : #{dataType => #{class@ => user_defined,
                                                                                                 type@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')])}}.
dataType_1 -> NAME '.' NAME '.' NAME                                           : #{dataType => #{class@ => user_defined,
                                                                                                 type@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3'), ".", unwrap_2_list('$5')])}}.
dataType_1 -> NCHAR         '(' INTNUM            ')'                          : #{dataType => #{class@ => sql,
                                                                                                 type@ => unwrap_2_list('$1'),
                                                                                                 size@ => unwrap_2_list('$3')}}.
dataType_1 -> NCLOB                                                            : #{dataType => #{class@ => sql,
                                                                                                 type@ => unwrap_2_list('$1')}}.
dataType_1 -> NUMBER                                                           : #{dataType => #{class@ => sql,
                                                                                                 type@ => unwrap_2_list('$1')}}.
dataType_1 -> NUMBER        '(' INTNUM            ')'                          : #{dataType => #{class@ => sql,
                                                                                                 type@ => unwrap_2_list('$1'),
                                                                                                 precision@ => unwrap_2_list('$3')}}.
dataType_1 -> NUMBER        '(' INTNUM ',' INTNUM ')'                          : #{dataType => #{class@ => sql,
                                                                                                 type@ => unwrap_2_list('$1'),
                                                                                                 precision@ => unwrap_2_list('$3'),
                                                                                                 scale@ =>  unwrap_2_list('$5')}}.
dataType_1 -> NVARCHAR2     '(' INTNUM            ')'                          : #{dataType => #{class@ => sql,
                                                                                                 type@ => unwrap_2_list('$1'),
                                                                                                 size@ => unwrap_2_list('$3')}}.
dataType_1 -> RAW                                                              : #{dataType => #{class@ => sql,
                                                                                                 type@ => unwrap_2_list('$1')}}.
dataType_1 -> RAW           '(' INTNUM            ')'                          : #{dataType => #{class@ => sql,
                                                                                                 type@ => unwrap_2_list('$1'),
                                                                                                 size@ => unwrap_2_list('$3')}}.
dataType_1 -> REF CURSOR                                                       : #{dataType => #{class@ => plsql,
                                                                                                 type@ => "REF CURSOR"}}.
dataType_1 -> ROWID                                                            : #{dataType => #{class@ => sql,
                                                                                                 type@ => unwrap_2_list('$1')}}.
dataType_1 -> TIMESTAMP                                                        : #{dataType => #{class@ => sql,
                                                                                                 type@ => unwrap_2_list('$1')}}.
dataType_1 -> TIMESTAMP                               WITH       TIME ZONE     : #{dataType => #{class@ => sql,
                                                                                                 type@ => unwrap_2_list('$1'),
                                                                                                 timeZone@ => true}}.
dataType_1 -> TIMESTAMP                               WITH LOCAL TIME ZONE     : #{dataType => #{class@ => sql,
                                                                                                 type@ => unwrap_2_list('$1'),
                                                                                                 timeZone@ => true,
                                                                                                 local@ => true}}.
dataType_1 -> TIMESTAMP     '(' INTNUM            ')'                          : #{dataType => #{class@ => sql,
                                                                                                 type@ => unwrap_2_list('$1'),
                                                                                                 precision@ => unwrap_2_list('$3')}}.
dataType_1 -> TIMESTAMP     '(' INTNUM            ')' WITH       TIME ZONE     : #{dataType => #{class@ => sql,
                                                                                                 type@ => unwrap_2_list('$1'),
                                                                                                 precision@ => unwrap_2_list('$3'),
                                                                                                 timeZone@ => true}}.
dataType_1 -> TIMESTAMP     '(' INTNUM            ')' WITH LOCAL TIME ZONE     : #{dataType => #{class@ => sql,
                                                                                                 type@ => unwrap_2_list('$1'),
                                                                                                 precision@ => unwrap_2_list('$3'),
                                                                                                 timeZone@ => true,
                                                                                                 local@ => true}}.
dataType_1 -> UROWID                                                           : #{dataType => #{class@ => sql,
                                                                                                 type@ => unwrap_2_list('$1')}}.
dataType_1 -> UROWID        '(' INTNUM            ')'                          : #{dataType => #{class@ => sql,
                                                                                                 type@ => unwrap_2_list('$1'),
                                                                                                 size@ => unwrap_2_list('$3')}}.
dataType_1 -> VARCHAR2                                                         : #{dataType => #{class@ => sql,
                                                                                                 type@ => unwrap_2_list('$1')}}.
dataType_1 -> VARCHAR2      '(' INTNUM BYTE       ')'                          : #{dataType => #{class@ => sql,
                                                                                                 type@ => unwrap_2_list('$1'),
                                                                                                 size@ => unwrap_2_list('$3'),
                                                                                                 sizeType@ => unwrap_2_list('$4')}}.
dataType_1 -> VARCHAR2      '(' INTNUM CHAR       ')'                          : #{dataType => #{class@ => sql,
                                                                                                 type@ => unwrap_2_list('$1'),
                                                                                                 size@ => unwrap_2_list('$3'),
                                                                                                 sizeType@ => unwrap_2_list('$4')}}.
dataType_1 -> XMLTYPE                                                          : #{dataType => #{class@ => sql,
                                                                                                 type@ => unwrap_2_list('$1')}}.

dataType_2 -> BINARY_INTEGER                                                   : #{dataType => #{class@ => plsql,
                                                                                                 type@ => unwrap_2_list('$1')}}.
dataType_2 -> NAME                   '%ROWTYPE'                                : #{dataType => #{class@ => user_defined,
                                                                                                 type@ => unwrap_2_list('$1'),
                                                                                                 attribute@ => unwrap_2_list('$2')}}.
dataType_2 -> NAME                   '%TYPE'                                   : #{dataType => #{class@ => user_defined,
                                                                                                 type@ => unwrap_2_list('$1'),
                                                                                                 attribute@ => unwrap_2_list('$2')}}.
dataType_2 -> NAME '.' NAME          '%ROWTYPE'                                : #{dataType => #{class@ => user_defined,
                                                                                                 type@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')]),
                                                                                                 attribute@ => unwrap_2_list('$4')}}.
dataType_2 -> NAME '.' NAME          '%TYPE'                                   : #{dataType => #{class@ => user_defined,
                                                                                                 type@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3')]),
                                                                                                 attribute@ => unwrap_2_list('$4')}}.
dataType_2 -> NAME '.' NAME '.' NAME '%TYPE'                                   : #{dataType => #{class@ => user_defined,
                                                                                                 type@ => lists:append([unwrap_2_list('$1'), ".", unwrap_2_list('$3'), ".", unwrap_2_list('$5')]),
                                                                                                 attribute@ => unwrap_2_list('$6')}}.
dataType_2 -> PLS_INTEGER                                                      : #{dataType => #{class@ => plsql,
                                                                                                 type@ => unwrap_2_list('$1')}}.
dataType_2 -> VARCHAR2      '(' INTNUM            ')'                          : #{dataType => #{class@ => sql,
                                                                                                 type@ => unwrap_2_list('$1'),
                                                                                                 size@ => unwrap_2_list('$3')}}.

dataType_3 -> LONG                                                             : #{dataType => #{class@ => sql,
                                                                                                 type@ => "LONG"}}.

fieldDefinition -> nameExtended dataType_1                   : #{fieldDefinition => #{name@ => '$1',
                                                                                      dataType@ => '$2'}}.
fieldDefinition -> nameExtended dataType_2                   : #{fieldDefinition => #{name@ => '$1',
                                                                                      dataType@ => '$2'}}.
fieldDefinition -> nameExtended dataType_1           default : #{fieldDefinition => #{name@ => '$1',
                                                                                      dataType@ => '$2',
                                                                                      default@ => '$3'}}.
fieldDefinition -> nameExtended dataType_2           default : #{fieldDefinition => #{name@ => '$1',
                                                                                      dataType@ => '$2',
                                                                                      default@ => '$3'}}.
fieldDefinition -> nameExtended dataType_1 NOT NULLX default : #{fieldDefinition => #{name@ => '$1',
                                                                                      dataType@ => '$2',
                                                                                      notNull@ => #{notNull => "not null"},
                                                                                      default@ => '$5'}}.
fieldDefinition -> nameExtended dataType_2 NOT NULLX default : #{fieldDefinition => #{name@ => '$1',
                                                                                      dataType@ => '$2',
                                                                                      notNull@ => #{notNull => "not null"},
                                                                                      default@ => '$5'}}.

packageFunctionDeclarationAttribute -> accessibleByClause    : #{packageFunctionDeclarationAttribute => '$1'}.
packageFunctionDeclarationAttribute -> DETERMINISTIC         : #{packageFunctionDeclarationAttribute => unwrap_2_list('$1')}.
packageFunctionDeclarationAttribute -> parallelEnabledClause : #{packageFunctionDeclarationAttribute => '$1'}.
packageFunctionDeclarationAttribute -> pipelinedClause       : #{packageFunctionDeclarationAttribute => '$1'}.
packageFunctionDeclarationAttribute -> resultCacheClause     : #{packageFunctionDeclarationAttribute => '$1'}.

parameterDeclarationCommaList -> parameterDeclaration                                   : ['$1'].
parameterDeclarationCommaList -> parameterDeclaration ',' parameterDeclarationCommaList : ['$1' | '$3'].

varrayTypeDef -> ARRAY         '(' INTNUM ')' OF dataType_1           : #{varrayTypeDef => #{type@ => #{varrayType => "array"},
                                                                                             size@ => #{varraySize => unwrap_2_list('$3')},
                                                                                             dataType@ => '$6'}}.
varrayTypeDef -> ARRAY         '(' INTNUM ')' OF dataType_2           : #{varrayTypeDef => #{type@ => #{varrayType => "array"},
                                                                                             size@ => #{varraySize => unwrap_2_list('$3')},
                                                                                             dataType@ => '$6'}}.
varrayTypeDef -> ARRAY         '(' INTNUM ')' OF dataType_1 NOT NULLX : #{varrayTypeDef => #{type@ => #{varrayType => "array"},
                                                                                             size@ => #{varraySize => unwrap_2_list('$3')},
                                                                                             dataType@ => '$6',
                                                                                             notNull@ => #{notNull => "not null"}}}.
varrayTypeDef -> ARRAY         '(' INTNUM ')' OF dataType_2 NOT NULLX : #{varrayTypeDef => #{type@ => #{varrayType => "array"},
                                                                                             size@ => #{varraySize => unwrap_2_list('$3')},
                                                                                             dataType@ => '$6',
                                                                                             notNull@ => #{notNull => "not null"}}}.
varrayTypeDef -> VARRAY        '(' INTNUM ')' OF dataType_1           : #{varrayTypeDef => #{type@ => #{varrayType => "varray"},
                                                                                             size@ => #{varraySize => unwrap_2_list('$3')},
                                                                                             dataType@ => '$6'}}.
varrayTypeDef -> VARRAY        '(' INTNUM ')' OF dataType_2           : #{varrayTypeDef => #{type@ => #{varrayType => "varray"},
                                                                                             size@ => #{varraySize => unwrap_2_list('$3')},
                                                                                             dataType@ => '$6'}}.
varrayTypeDef -> VARRAY        '(' INTNUM ')' OF dataType_1 NOT NULLX : #{varrayTypeDef => #{type@ => #{varrayType => "varray"},
                                                                                             size@ => #{varraySize => unwrap_2_list('$3')},
                                                                                             dataType@ => '$6',
                                                                                             notNull@ => #{notNull => "not null"}}}.
varrayTypeDef -> VARRAY        '(' INTNUM ')' OF dataType_2 NOT NULLX : #{varrayTypeDef => #{type@ => #{varrayType => "varray"},
                                                                                             size@ => #{varraySize => unwrap_2_list('$3')},
                                                                                             dataType@ => '$6',
                                                                                             notNull@ => #{notNull => "not null"}}}.
varrayTypeDef -> VARYING ARRAY '(' INTNUM ')' OF dataType_1           : #{varrayTypeDef => #{type@ => #{varrayType => "varying array"},
                                                                                             size@ => #{varraySize => unwrap_2_list('$4')},
                                                                                             dataType@ => '$7'}}.
varrayTypeDef -> VARYING ARRAY '(' INTNUM ')' OF dataType_2           : #{varrayTypeDef => #{type@ => #{varrayType => "varying array"},
                                                                                             size@ => #{varraySize => unwrap_2_list('$4')},
                                                                                             dataType@ => '$7'}}.
varrayTypeDef -> VARYING ARRAY '(' INTNUM ')' OF dataType_1 NOT NULLX : #{varrayTypeDef => #{type@ => #{varrayType => "varying array"},
                                                                                             size@ => #{varraySize => unwrap_2_list('$4')},
                                                                                             dataType@ => '$7',
                                                                                             notNull@ => #{notNull => "not null"}}}.
varrayTypeDef -> VARYING ARRAY '(' INTNUM ')' OF dataType_2 NOT NULLX : #{varrayTypeDef => #{type@ => #{varrayType => "varying array"},
                                                                                             size@ => #{varraySize => unwrap_2_list('$4')},
                                                                                             dataType@ => '$7',
                                                                                             notNull@ => #{notNull => "not null"}}}.

%% Level 10 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nameExtended -> API_GROUP : "api_group".
nameExtended -> FALSE     : "false".
nameExtended -> NAME      : unwrap_2_list('$1').
nameExtended -> NONE      : "none".
nameExtended -> TRUE      : "true".

parameterDeclaration -> NAME               dataType_1         : #{parameterDeclaration => #{name@ => unwrap_2_list('$1'),
                                                                                            dataType@ => '$2'}}.
parameterDeclaration -> NAME               dataType_1 default : #{parameterDeclaration => #{name@ => unwrap_2_list('$1'),
                                                                                            dataType@ => '$2',
                                                                                            default@ => '$3'}}.
parameterDeclaration -> NAME               dataType_2         : #{parameterDeclaration => #{name@ => unwrap_2_list('$1'),
                                                                                            dataType@ => '$2'}}.
parameterDeclaration -> NAME               dataType_2 default : #{parameterDeclaration => #{name@ => unwrap_2_list('$1'),
                                                                                            dataType@ => '$2',
                                                                                            default@ => '$3'}}.
parameterDeclaration -> NAME IN            dataType_1         : #{parameterDeclaration => #{name@ => unwrap_2_list('$1'),
                                                                                            mode@ => unwrap_2_list('$2'),
                                                                                            dataType@ => '$3'}}.
parameterDeclaration -> NAME IN            dataType_1 default : #{parameterDeclaration => #{name@ => unwrap_2_list('$1'),
                                                                                            mode@ => unwrap_2_list('$2'),
                                                                                            dataType@ => '$3',
                                                                                            default@ => '$4'}}.
parameterDeclaration -> NAME IN            dataType_2         : #{parameterDeclaration => #{name@ => unwrap_2_list('$1'),
                                                                                            mode@ => unwrap_2_list('$2'),
                                                                                            dataType@ => '$3'}}.
parameterDeclaration -> NAME IN            dataType_2 default : #{parameterDeclaration => #{name@ => unwrap_2_list('$1'),
                                                                                            mode@ => unwrap_2_list('$2'),
                                                                                            dataType@ => '$3',
                                                                                            default@ => '$4'}}.
parameterDeclaration -> NAME    OUT        dataType_1         : #{parameterDeclaration => #{name@ => unwrap_2_list('$1'),
                                                                                            mode@ => unwrap_2_list('$2'),
                                                                                            dataType@ => '$3'}}.
parameterDeclaration -> NAME    OUT        dataType_2         : #{parameterDeclaration => #{name@ => unwrap_2_list('$1'),
                                                                                            mode@ => unwrap_2_list('$2'),
                                                                                            dataType@ => '$3'}}.
parameterDeclaration -> NAME    OUT NOCOPY dataType_1         : #{parameterDeclaration => #{name@ => unwrap_2_list('$1'),
                                                                                            mode@ => unwrap_2_list('$2'),
                                                                                            nocopy@ => unwrap_2_list('$3'),
                                                                                            dataType@ => '$4'}}.
parameterDeclaration -> NAME    OUT NOCOPY dataType_2         : #{parameterDeclaration => #{name@ => unwrap_2_list('$1'),
                                                                                            mode@ => unwrap_2_list('$2'),
                                                                                            nocopy@ => unwrap_2_list('$3'),
                                                                                            dataType@ => '$4'}}.
parameterDeclaration -> NAME IN OUT        dataType_1         : #{parameterDeclaration => #{name@ => unwrap_2_list('$1'),
                                                                                            mode@ => "IN OUT",
                                                                                            dataType@ => '$4'}}.
parameterDeclaration -> NAME IN OUT        dataType_2         : #{parameterDeclaration => #{name@ => unwrap_2_list('$1'),
                                                                                            mode@ => "IN OUT",
                                                                                            dataType@ => '$4'}}.
parameterDeclaration -> NAME IN OUT NOCOPY dataType_1         : #{parameterDeclaration => #{name@ => unwrap_2_list('$1'),
                                                                                            mode@ => "IN OUT",
                                                                                            nocopy@ => unwrap_2_list('$4'),
                                                                                            dataType@ => '$5'}}.
parameterDeclaration -> NAME IN OUT NOCOPY dataType_2         : #{parameterDeclaration => #{name@ => unwrap_2_list('$1'),
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

pipelinedClause -> PIPELINED                                       : #{pipelinedClause => #{}}.
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
%% Copyright (c) 2018-20 Konnexions GmbH.  All Rights Reserved.
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
            % ?E("~n ParseTree: ~p~n Tokens: ~p~n", [ParseTree, _Tokens]),
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
                    % ?E("~n ParseTree: ~p~n Tokens: ~p~n", [PTree, Toks]),
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
