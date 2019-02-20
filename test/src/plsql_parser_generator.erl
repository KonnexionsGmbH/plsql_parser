%% -----------------------------------------------------------------------------
%%
%% plsql_parser_generator.erl: PL/SQL - test data generator.
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

-module(plsql_parser_generator).

-export([generate/0]).

-define(NODEBUG, true).

-include("plsql_parser_generator.hrl").

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate Test Data.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate() ->
    ets:new(?CODE_TEMPLATES, [
        named_table,
        private,
        ordered_set
    ]),

    create_code(),

    %% Common tests ............................................................

    case ?GENERATE_CT of
        true ->
            case ?GENERATE_PERFORMANCE of
                true ->
                    ok = file_create_ct_all("performance", "complete_",
                        "compacted", ?ALL_CLAUSE_PERFORMANCE);
                _ -> ok
            end,
            case ?GENERATE_RELIABILITY of
                true ->
                    case ?GENERATE_COMPACTED of
                        true ->
                            ok = file_create_ct_all("reliability", "complete_",
                                "compacted", ?ALL_CLAUSE_RELIABILITY);
                        _ ->
                            ok = file_create_ct_all("reliability", "complete_",
                                "detailed_", ?ALL_CLAUSE_RELIABILITY)
                    end;
                _ -> ok
            end;
        _ -> ok
    end,

    %% EUnit tests .............................................................

    case ?GENERATE_EUNIT of
        true ->
            case ?GENERATE_RELIABILITY of
                true ->
                    ok = file_create_eunit_all("reliability", "complete_",
                        ?ALL_CLAUSE_RELIABILITY),
                    ok = file_create_eunit_all("reliability", "complete_",
                        ?ALL_CLAUSE_RELIABILITY_SQL_DETAILED);
                _ -> ok
            end;
        _ -> ok
    end.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Creating code base.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code() ->

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 01
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format(user, "~n" ++ ?MODULE_STRING ++
        " : ======================================================> create_code: Level 01   <===================~n",
        [])),

    % based on lexer definitions ...............................................
    create_code(approxnum),
    create_code(comparison),
    create_code(dataType_3),
    create_code(intnum),
    create_code(man_page),
    create_code(name),
    create_code(parameter),
    create_code(string),
    % based on parser definitions ..............................................
    create_code(apiHiddenAnnotation),
    create_code(defaultCollationClause),
    create_code(invokerRightsClause),
    create_code(objectPrivilegeType),
    create_code(sharingClause),
    create_code(sqlplusCommand),
    create_code(systemPrivilegeType),
    create_code(unaryAddOrSubtract),
    create_code(unitKind),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 02
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format(user, "~n" ++ ?MODULE_STRING ++
        " : ======================================================> create_code: Level 02   <===================~n",
        [])),

    create_code(accessor),
    create_code(apiGroupAnnotation),
    create_code(columnRef),
    create_code(dataSource),
    create_code(dataType_1),
    create_code(dataType_2),
    create_code(dataTypeReturn),
    create_code(exceptionDeclaration),
    create_code(literal),
    create_code(nameExtended),
    create_code(objectPrivilegeAnnotation),
    create_code(parameterRef),
    create_code(pipelinedClause),
    create_code(refCursorTypeDefinition),
    create_code(systemPrivilegeAnnotation),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 03
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format(user, "~n" ++ ?MODULE_STRING ++
        " : ======================================================> create_code: Level 03   <===================~n",
        [])),

    create_code(accessorCommaList),
    create_code(assocArrayTypeDef),
    create_code(columnRefCommaList),
    create_code(dataSourceCommaList),
    create_code(privilegeAnnotationList),
    create_code(subtypeDefinition),
    create_code(varrayTypeDef),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 04
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format(user, "~n" ++ ?MODULE_STRING ++
        " : ======================================================> create_code: Level 04   <===================~n",
        [])),

    create_code(accessibleByClause),
    create_code(collectionTypeDefinition),
    create_code(functionAnnotation),
    create_code(procedureAnnotation),
    create_code(resultCacheClause),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 05
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format(user, "~n" ++ ?MODULE_STRING ++
        " : ======================================================> create_code: Level 05   <===================~n",
        [])),

    create_code(plsqlPackageSourceAttribute),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 06
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format(user, "~n" ++ ?MODULE_STRING ++
        " : ======================================================> create_code: Level 06   <===================~n",
        [])),

    create_code(plsqlPackageSourceAttributeList),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 07 = 11-12/1
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format(user, "~n" ++ ?MODULE_STRING ++
        " : ======================================================> create_code: Level 07   <===================~n",
        [])),

    create_code_layer("1"),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 08 = 11-12/2
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format(user, "~n" ++ ?MODULE_STRING ++
        " : ======================================================> create_code: Level 08   <===================~n",
        [])),

    create_code_layer("2"),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 31
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format(user, "~n" ++ ?MODULE_STRING ++
        " : ======================================================> create_code: Level 31   <===================~n",
        [])),

    create_code(expression),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 32
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format(user, "~n" ++ ?MODULE_STRING ++
        " : ======================================================> create_code: Level 32   <===================~n",
        [])),

    create_code(parameterDeclaration),
    create_code(streamingClause),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 33
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format(user, "~n" ++ ?MODULE_STRING ++
        " : ======================================================> create_code: Level 33   <===================~n",
        [])),

    create_code(parallelEnabledClause),
    create_code(parameterDeclarationCommaList),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 34
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format(user, "~n" ++ ?MODULE_STRING ++
        " : ======================================================> create_code: Level 34   <===================~n",
        [])),

    create_code(functionHeading),
    create_code(packageFunctionDeclarationAttribute),
    create_code(procedureHeading),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 35
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format(user, "~n" ++ ?MODULE_STRING ++
        " : ======================================================> create_code: Level 35   <===================~n",
        [])),

    create_code(packageFunctionDeclarationAttributeList),
    create_code(packageProcedureDeclaration),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 36
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format(user, "~n" ++ ?MODULE_STRING ++
        " : ======================================================> create_code: Level 36   <===================~n",
        [])),

    create_code(packageFunctionDeclaration),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 37
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format(user, "~n" ++ ?MODULE_STRING ++
        " : ======================================================> create_code: Level 37   <===================~n",
        [])),

    create_code(packageItem),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 38
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format(user, "~n" ++ ?MODULE_STRING ++
        " : ======================================================> create_code: Level 38   <===================~n",
        [])),

    create_code(packageItemConditional),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 39
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format(user, "~n" ++ ?MODULE_STRING ++
        " : ======================================================> create_code: Level 39   <===================~n",
        [])),

    create_code(packageItemList),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 40
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format(user, "~n" ++ ?MODULE_STRING ++
        " : ======================================================> create_code: Level 40   <===================~n",
        [])),

    create_code(plsqlPackageSource),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 80
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format(user, "~n" ++ ?MODULE_STRING ++
        " : ======================================================> create_code: Level 80   <===================~n",
        [])),

    create_code(createPackage),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 99
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format(user, "~n" ++ ?MODULE_STRING ++
        " : ======================================================> create_code: Level 99   <===================~n",
        [])),

    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Creating code layered.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code_layer(Version) ->

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 11
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format(user, "~n" ++ ?MODULE_STRING ++
        " : ================================================> create_code_layer: Level 11/~s <===================~n",
        [Version])),

    create_code(expression),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 12
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format(user, "~n" ++ ?MODULE_STRING ++
        " : ================================================> create_code_layer: Level 12/~s <===================~n",
        [Version])),

    create_code(default),
    create_code(functionArg),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 13
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format(user, "~n" ++ ?MODULE_STRING ++
        " : ================================================> create_code_layer: Level 13/~s <===================~n",
        [Version])),

    create_code(constantDeclaration),
    create_code(fieldDefinition),
    create_code(functionArgCommaList),
    create_code(variableDeclaration),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 14
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format(user, "~n" ++ ?MODULE_STRING ++
        " : ================================================> create_code_layer: Level 14/~s <===================~n",
        [Version])),

    create_code(fieldDefinitionCommaList),
    create_code(functionRef),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 15
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format(user, "~n" ++ ?MODULE_STRING ++
        " : ================================================> create_code_layer: Level 15/~s <===================~n",
        [Version])),

    create_code(recordTypeDefinition).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Adding parentheses to a query_spec.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bracket_query_spec(Expression) ->
    case string:substr(Expression, 1, 7) == "Select " of
        true -> lists:append(["(", Expression, ")"]);
        _ -> Expression
    end.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% accessor ::= unitKind? ( NAME '.' )?  NAME
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(accessor = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),
    [{unitKind, UnitKind}] = ets:lookup(?CODE_TEMPLATES, unitKind),
    UnitKind_Length = length(UnitKind),

    Code =
        [
            lists:append(
                [
                    case rand:uniform(2) rem 2 of
                        1 ->
                            lists:nth(rand:uniform(UnitKind_Length),
                                UnitKind) ++ " ";
                        _ -> []
                    end,
                    case rand:uniform(2) rem 2 of
                        1 -> lists:nth(rand:uniform(Name_Length), Name) ++ ".";
                        _ -> []
                    end,
                    lists:nth(rand:uniform(Name_Length), Name)
                ])
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% accessibleByClause ::= 'ACCESSIBLE' 'BY' '(' accessor+ ')'
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(accessibleByClause = Rule) ->
    ?CREATE_CODE_START,
    [{accessorCommaList, AccessorCommaList}] = ets:lookup(?CODE_TEMPLATES,
        accessorCommaList),
    AccessorCommaList_Length = length(AccessorCommaList),

    Code =
        [
            lists:append(
                [
                    "Accessible By (",
                    lists:nth(rand:uniform(AccessorCommaList_Length),
                        AccessorCommaList),
                    ")"
                ])
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% accessorCommaList ::= accessor ( ',' accessor )*
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(accessorCommaList = Rule) ->
    ?CREATE_CODE_START,
    [{accessor, Accessor}] = ets:lookup(?CODE_TEMPLATES, accessor),
    Accessor_Length = length(Accessor),

    Code =
        [
            case rand:uniform(2) rem 2 of
                1 -> lists:append(
                    [
                        lists:nth(rand:uniform(Accessor_Length), Accessor),
                        ",",
                        lists:nth(rand:uniform(Accessor_Length), Accessor)
                    ]);
                _ -> lists:nth(rand:uniform(Accessor_Length), Accessor)
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% apiGroupAnnotation ::= '--<>' 'API_GROUP' '=' NAME
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(apiGroupAnnotation = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),

    Code =
        [
            lists:append(
                [
                    "--<> API_Group = ",
                    lists:nth(rand:uniform(Name_Length), Name),
                    ?CHAR_NEWLINE
                ])
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% apiHidenAnnotation ::= '--<>' 'API_HIDDEN' '=' 'TRUE'
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(apiHiddenAnnotation = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
                "--<> API_Hidden = True" ++ ?CHAR_NEWLINE
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (([\.][0-9]+)|([0-9]+[\.]?[0-9]*))[eE]?[+-]?[0-9]*[fFdD]?)
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(approxnum = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            "0.5",
            "0.5d",
            "0.5e1",
            "0.5e2d",
            "05d",
            "1.0",
            "1.1D",
            "1D",
            "1e3",
            "1e4D",
            "2.5",
            "2.5e-03",
            "2.5f",
            "2.5F",
            "25e5",
            "25e-03",
            "25e-03d",
            "25e6f",
            "25f",
            "6.34",
            "6.34e7",
            "6.34e8F",
            "6.34F",
            "63.4f",
            "634F"
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% assocArrayTypeDef ::= 'TABLE' 'OF' ( dataType_1 | dataType_2 ) ( NOT NULL )? ( 'INDEX' 'BY' ( dataType_2 | dataType_3 ) )?
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(assocArrayTypeDef = Rule) ->
    ?CREATE_CODE_START,
    [{dataType_1, DataType_1}] = ets:lookup(?CODE_TEMPLATES, dataType_1),
    DataType_1_Length = length(DataType_1),
    [{dataType_2, DataType_2}] = ets:lookup(?CODE_TEMPLATES, dataType_2),
    DataType_2_Length = length(DataType_2),
    [{dataType_3, DataType_3}] = ets:lookup(?CODE_TEMPLATES, dataType_3),
    DataType_3_Length = length(DataType_3),

    Code =
        [
            lists:append(
                [
                    "table of  ",
                    case rand:uniform(5) rem 5 of
                        1 -> lists:nth(rand:uniform(DataType_2_Length),
                            DataType_2);
                        _ -> lists:nth(rand:uniform(DataType_1_Length),
                            DataType_1)
                    end,
                    case rand:uniform(5) rem 5 of
                        1 -> " not null ";
                        _ -> []
                    end,
                    case rand:uniform(2) rem 2 of
                        1 -> " index by "
                        ++ case rand:uniform(10) rem 10 of
                               1 -> lists:nth(rand:uniform(DataType_3_Length),
                                   DataType_3);
                               _ -> lists:nth(rand:uniform(DataType_2_Length),
                                   DataType_2)
                           end;
                        _ -> []
                    end
                ])
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% collectionTypeDefinition ::= 'TYPE' NAME 'IS' ( assocArrayTypeDef | varrayTypeDef ) ';'
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(collectionTypeDefinition = Rule) ->
    ?CREATE_CODE_START,
    [{assocArrayTypeDef, AssocArrayTypeDef}] =
        ets:lookup(?CODE_TEMPLATES, assocArrayTypeDef),
    AssocArrayTypeDef_Length = length(AssocArrayTypeDef),
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),
    [{varrayTypeDef, VarrayTypeDef}] =
        ets:lookup(?CODE_TEMPLATES, varrayTypeDef),
    VarrayTypeDef_Length = length(VarrayTypeDef),

    Code =
        [
            lists:append(
                [
                    "type ",
                    lists:nth(rand:uniform(Name_Length), Name),
                    " is ",
                    case rand:uniform(2) rem 2 of
                        1 -> lists:nth(rand:uniform(AssocArrayTypeDef_Length),
                            AssocArrayTypeDef);
                        _ -> lists:nth(rand:uniform(VarrayTypeDef_Length),
                            VarrayTypeDef)
                    end,
                    ";"
                ])
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% columnRef ::= ( ( ( NAME '.' )? NAME '.' )? NAME ( '(' '+' ')' )? )
%%             | ( ( NAME '.' )? NAME '.' '*' )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(columnRef = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),

    Code =
        [
            case rand:uniform(2) rem 2 of
                1 -> lists:append(
                    [
                        lists:nth(rand:uniform(Name_Length), Name),
                        ".",
                        lists:nth(rand:uniform(Name_Length), Name),
                        case rand:uniform(3) rem 3 of
                            1 -> " (+)";
                            2 -> ".*";
                            _ -> []
                        end
                    ]);
                _ -> lists:nth(rand:uniform(Name_Length), Name) ++
                case rand:uniform(3) rem 3 of
                    1 -> " (+)";
                    2 -> ".*";
                    _ -> []
                end
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    store_code(expression, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cclumnRefCommalist ::= columnRef ( ',' columnRef )*
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(columnRefCommaList = Rule) ->
    ?CREATE_CODE_START,
    [{columnRef, ColumnRef}] = ets:lookup(?CODE_TEMPLATES, columnRef),
    ColumnRef_Length = length(ColumnRef),

    Code =
        [
            case rand:uniform(2) rem 2 of
                1 -> lists:append(
                    [
                        lists:nth(rand:uniform(ColumnRef_Length), ColumnRef),
                        ",",
                        lists:nth(rand:uniform(ColumnRef_Length), ColumnRef)
                    ]);
                _ -> lists:nth(rand:uniform(ColumnRef_Length), ColumnRef)
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (=|!=|^=|<>|<|>|<=|>=)
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(comparison = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            "=",
            "!=",
            "^=",
            "<>",
            "<",
            ">",
            "<=",
            ">="
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% constantDeclaration ::= NAME 'CONSTANT' ( dataType_1 | dataType_2 ) ( 'NOT' 'NULL' )? default ';'
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(constantDeclaration = Rule) ->
    ?CREATE_CODE_START,
    [{dataType_1, DataType_1}] = ets:lookup(?CODE_TEMPLATES, dataType_1),
    DataType_1_Length = length(DataType_1),
    [{dataType_2, DataType_2}] = ets:lookup(?CODE_TEMPLATES, dataType_2),
    DataType_2_Length = length(DataType_2),
    [{default, Default}] = ets:lookup(?CODE_TEMPLATES, default),
    Default_Length = length(Default),
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),

    Code =
        [
            lists:append(
                [
                    lists:nth(rand:uniform(Name_Length), Name),
                    " constant ",
                    case rand:uniform(5) rem 5 of
                        1 -> lists:nth(rand:uniform(DataType_2_Length),
                            DataType_2);
                        _ -> lists:nth(rand:uniform(DataType_1_Length),
                            DataType_1)
                    end,
                    case rand:uniform(2) rem 2 of
                        1 -> " not null ";
                        _ -> []
                    end,
                    lists:nth(rand:uniform(Default_Length), Default),
                    ";"
                ])
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% createPackage ::= 'CREATE' ( 'OR' 'REPLACE' )? ( 'EDITIONABLE' | 'NONEDITIONABLE' )?
%%                              'PACKAGE' plsqlPackageSource ';' '/'?
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(createPackage = Rule) ->
    ?CREATE_CODE_START,
    [{plsqlPackageSource, PlsqlPackageSource}] = ets:lookup(?CODE_TEMPLATES,
        plsqlPackageSource),
    PlsqlPackageSource_Length = length(PlsqlPackageSource),
    [{sqlplusCommand, SqlplusCommand}] = ets:lookup(?CODE_TEMPLATES,
        sqlplusCommand),
    SqlplusCommand_Length = length(SqlplusCommand),

    Code =
        [
            lists:append(
                [
                    case rand:uniform(5) rem 5 of
                        1 -> lists:nth(rand:uniform(SqlplusCommand_Length),
                            SqlplusCommand);
                        _ -> []
                    end,
                    "Create",
                    case rand:uniform(2) rem 2 of
                        1 -> " Or Replace";
                        _ -> []
                    end,
                    case rand:uniform(3) rem 3 of
                        1 -> " Editionable";
                        2 -> " Noneditionable";
                        _ -> []
                    end,
                    " Package ",
                    lists:nth(rand:uniform(PlsqlPackageSource_Length),
                        PlsqlPackageSource),
                    ";",
                    case rand:uniform(2) rem 2 of
                        1 -> "/";
                        _ -> []
                    end
                ])
            || _ <- lists:seq(1, ?MAX_PLSQL * 2)
        ],
    store_code(Rule, Code, ?MAX_PLSQL, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% dataSource ::= ( NAME '.' )? NAME
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(dataSource = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),

    Code =
        [
            lists:append(
                [
                    case rand:uniform(2) rem 2 of
                        1 -> lists:nth(rand:uniform(Name_Length), Name) ++ ".";
                        _ -> []
                    end,
                    lists:nth(rand:uniform(Name_Length), Name)
                ])
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% dataSourceCommaList ::= dataSource ( ',' dataSource )*
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(dataSourceCommaList = Rule) ->
    ?CREATE_CODE_START,
    [{dataSource, DataSource}] = ets:lookup(?CODE_TEMPLATES, dataSource),
    DataSource_Length = length(DataSource),

    Code =
        [
            case rand:uniform(2) rem 2 of
                1 -> lists:append(
                    [
                        lists:nth(rand:uniform(DataSource_Length), DataSource),
                        ",",
                        lists:nth(rand:uniform(DataSource_Length), DataSource)
                    ]);
                _ -> lists:nth(rand:uniform(DataSource_Length), DataSource)
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% dataType_1 ::= 'BFILE'
%%              | 'BINARY_DOUBLE'
%%              | 'BINARY_FLOAT'
%%              | 'BLOB'
%%              | 'BOOLEAN'
%%              | ( 'CHAR' ( '(' INTNUM ( 'BYTE' | 'CHAR' )? ')' )? )
%%              | 'CLOB'
%%              | 'DATE'
%%              | ( ( 'FLOAT' | 'UROWID' ) ( '(' INTNUM ')' )? )
%%              | ( 'INTERVAL' 'DAY' ( '(' INTNUM ')' )? 'TO' 'SECOND' ( '(' INTNUM ')' )? )
%%              | ( 'INTERVAL' 'YEAR' ( '(' INTNUM ')' )? 'TO' 'MONTH' )
%%              | ( 'LONG' 'RAW' )
%%              | ( ( 'NCHAR' | 'NVARCHAR2' | 'RAW' ) '(' INTNUM ')' )
%%              | 'NCLOB'
%%              | ( 'NUMBER' ( '(' INTNUM ( ',' INTNUM )? ')' )? )
%%              | 'RAW'
%%              | ( 'REF' 'CURSOR' )
%%              | 'ROWID'
%%              | ( 'TIMESTAMP' ( '(' INTNUM ')' )? ( 'WITH' 'LOCAL'? 'TIME' 'ZONE' )? )
%%              | ( 'VARCHAR2' ( '(' INTNUM ( 'BYTE' | 'CHAR' ) ')' )? )
%%              | 'XMLTYPE'
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(dataType_1 = Rule) ->
    ?CREATE_CODE_START,
    [{intnum, Intnum}] = ets:lookup(?CODE_TEMPLATES, intnum),
    Intnum_Length = length(Intnum),
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),

    Code =
        [
            "Bfile",
            "Binary_double",
            "Binary_float",
            "Blob",
            "Boolean",
            "Char",
            lists:append(
                [
                    "Char(", lists:nth(rand:uniform(Intnum_Length),
                    Intnum),
                    ")"
                ]),
            lists:append(
                [
                    "Char(",
                    lists:nth(rand:uniform(Intnum_Length), Intnum),
                    " Byte)"
                ]),
            lists:append(
                [
                    "Char(",
                    lists:nth(rand:uniform(Intnum_Length), Intnum),
                    " Char)"
                ]),
            "Clob",
            "Date",
            "Float",
            lists:append(
                [
                    "Float(",
                    lists:nth(rand:uniform(Intnum_Length), Intnum),
                    ")"
                ]),
            "Interval Day To Second",
            lists:append(
                [
                    "Interval Day To Second(",
                    lists:nth(rand:uniform(Intnum_Length), Intnum),
                    ")"
                ]),
            lists:append(
                [
                    "Interval Day(",
                    lists:nth(rand:uniform(Intnum_Length), Intnum),
                    ")",
                    "To Second(",
                    lists:nth(rand:uniform(Intnum_Length), Intnum),
                    ")"
                ]),
            lists:append(
                [
                    "Interval Day(",
                    lists:nth(rand:uniform(Intnum_Length), Intnum),
                    ")",
                    "To Second(",
                    lists:nth(rand:uniform(Intnum_Length), Intnum),
                    ")"
                ]),
            "Interval Year To Month",
            lists:append(
                [
                    "Interval Year(",
                    lists:nth(rand:uniform(Intnum_Length), Intnum),
                    ")",
                    "To Month"
                ]),
            "Long Raw",
            lists:nth(rand:uniform(Name_Length), Name),
            lists:append(
                [
                    lists:nth(rand:uniform(Name_Length), Name),
                    ".",
                    lists:nth(rand:uniform(Name_Length), Name)
                ]),
            lists:append(
                [
                    lists:nth(rand:uniform(Name_Length), Name),
                    ".",
                    lists:nth(rand:uniform(Name_Length), Name),
                    ".",
                    lists:nth(rand:uniform(Name_Length), Name)
                ]),
            lists:append(
                [
                    "Nchar(",
                    lists:nth(rand:uniform(Intnum_Length), Intnum),
                    ")"
                ]),
            "Nclob",
            "Number",
            lists:append(
                [
                    "Number(",
                    lists:nth(rand:uniform(Intnum_Length), Intnum),
                    ")"
                ]),
            lists:append(
                [
                    "Number(",
                    lists:nth(rand:uniform(Intnum_Length), Intnum),
                    ",",
                    lists:nth(rand:uniform(Intnum_Length), Intnum),
                    ")"
                ]),
            lists:append(
                [
                    "Nvarchar2(",
                    lists:nth(rand:uniform(Intnum_Length), Intnum),
                    ")"
                ]),
            lists:append(
                [
                    "Raw(",
                    lists:nth(rand:uniform(Intnum_Length), Intnum),
                    ")"
                ]),
            "Raw",
            "Ref Cursor",
            "Rowid",
            "Timestamp",
            "Timestamp With Time Zone",
            "Timestamp With Local Time Zone",
            lists:append(
                [
                    "Timestamp(",
                    lists:nth(rand:uniform(Intnum_Length), Intnum),
                    ")"
                ]),
            lists:append(
                [
                    "Timestamp(",
                    lists:nth(rand:uniform(Intnum_Length), Intnum),
                    ")With Time Zone"
                ]),
            lists:append(
                [
                    "Timestamp(",
                    lists:nth(rand:uniform(Intnum_Length), Intnum),
                    ")With Local Time Zone"
                ]),
            "Urowid",
            lists:append(
                [
                    "Urowid(",
                    lists:nth(rand:uniform(Intnum_Length), Intnum),
                    ")"
                ]),
            "Varchar2",
            lists:append(
                [
                    "Varchar2(",
                    lists:nth(rand:uniform(Intnum_Length), Intnum),
                    "Byte)"
                ]),
            lists:append(
                [
                    "Varchar2(",
                    lists:nth(rand:uniform(Intnum_Length), Intnum),
                    "Char)"
                ]),
            "Xmltype"
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% dataType_2 ::= 'BINARY_INTEGER'
%%              | ( NAME ( '%ROWTYPE' | '%TYPE' )? )
%%              | ( ( ( NAME '.' )? NAME '.' )? NAME '%TYPE'? )
%%              | 'PLS_INTEGER'
%%              | ( 'VARCHAR2' '(' INTNUM ')' )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(dataType_2 = Rule) ->
    ?CREATE_CODE_START,
    [{intnum, Intnum}] = ets:lookup(?CODE_TEMPLATES, intnum),
    Intnum_Length = length(Intnum),
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),

    Code =
        [
            "Binary_integer",
                lists:nth(rand:uniform(Name_Length), Name) ++ "%Rowtype",
                lists:nth(rand:uniform(Name_Length), Name) ++ "%Type",
            lists:append(
                [
                    lists:nth(rand:uniform(Name_Length), Name),
                    ".",
                    lists:nth(rand:uniform(Name_Length), Name),
                    "%Rowtype"
                ]),
            lists:append(
                [
                    lists:nth(rand:uniform(Name_Length), Name),
                    ".",
                    lists:nth(rand:uniform(Name_Length), Name),
                    "%Type"
                ]),
            lists:append(
                [
                    lists:nth(rand:uniform(Name_Length), Name),
                    ".",
                    lists:nth(rand:uniform(Name_Length), Name),
                    ".",
                    lists:nth(rand:uniform(Name_Length), Name),
                    "%Type"
                ]),
            "Pls_integer",
            lists:append(
                [
                    "Varchar2(",
                    lists:nth(rand:uniform(Intnum_Length), Intnum),
                    ")"
                ])
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% dataType_3 ::= 'LONG'
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(dataType_3 = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            "Long"
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% dataTypeReturn ::= 'BFILE'
%%                  | 'BINARY_DOUBLE'
%%                  | 'BINARY_FLOAT'
%%                  | 'BINARY_INTEGER'
%%                  | 'BLOB'
%%                  | 'BOOLEAN'
%%                  | 'CHAR'
%%                  | 'CLOB'
%%                  | 'DATE'
%%                  | 'FLOAT'
%%                  | ( 'INTERVAL' 'DAY' ( '(' INTNUM ')' )? 'TO' 'SECOND' ( '(' INTNUM ')' )? )
%%                  | ( 'INTERVAL' 'YEAR' ( '(' INTNUM ')' )? 'TO' 'MONTH' )
%%                  | ( 'LONG' 'RAW' )
%%                  | ( ( ( NAME '.' )? NAME '.' )? NAME '%TYPE'? )
%%                  | ( NAME ( '%ROWTYPE' | '%TYPE' )? )
%%                  | 'NCLOB'
%%                  | 'NUMBER'
%%                  | 'PLS_INTEGER'
%%                  | ( 'REF' 'CURSOR' )
%%                  | 'ROWID'
%%                  | ( 'TIMESTAMP' ( '(' INTNUM ')' )? ( 'WITH' 'LOCAL'? 'TIME' 'ZONE' )? )
%%                  | 'UROWID'
%%                  | 'VARCHAR2'
%%                  | 'XMLTYPE'
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(dataTypeReturn = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),

    Code =
        [
            "Bfile",
            "Binary_double",
            "Binary_float",
            "Binary_integer",
            "Blob",
            "Boolean",
            "Char",
            "Clob",
            "Date",
            "Float",
            "Interval Day To Second",
            "Interval Year To Month",
            "Long Raw",
            lists:nth(rand:uniform(Name_Length), Name),
                lists:nth(rand:uniform(Name_Length), Name) ++ "%Rowtype",
                lists:nth(rand:uniform(Name_Length), Name) ++ "%Type",
            lists:append(
                [
                    lists:nth(rand:uniform(Name_Length), Name),
                    ".",
                    lists:nth(rand:uniform(Name_Length), Name)
                ]),
            lists:append(
                [
                    lists:nth(rand:uniform(Name_Length), Name),
                    ".",
                    lists:nth(rand:uniform(Name_Length), Name),
                    "%Type"
                ]),
            lists:append(
                [
                    lists:nth(rand:uniform(Name_Length), Name),
                    ".",
                    lists:nth(rand:uniform(Name_Length), Name),
                    ".",
                    lists:nth(rand:uniform(Name_Length), Name)
                ]),
            lists:append(
                [
                    lists:nth(rand:uniform(Name_Length), Name),
                    ".",
                    lists:nth(rand:uniform(Name_Length), Name),
                    ".",
                    lists:nth(rand:uniform(Name_Length), Name),
                    "%Type"
                ]),
            "Nclob",
            "Number",
            "Pls_integer",
            "Ref Cursor",
            "Rowid",
            "Timestamp",
            "Timestamp With Time Zone",
            "Timestamp With Local Time Zone",
            "Varchar2",
            "Xmltype"
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% default ::= ( ':=' | 'DEFAULT' ) expression
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(default = Rule) ->
    ?CREATE_CODE_START,
    [{expression, Expression}] = ets:lookup(?CODE_TEMPLATES, expression),
    Expression_Length = length(Expression),

    Code =
        [
                case rand:uniform(2) rem 2 of
                    1 -> " := ";
                    _ -> " Default "
                end ++
                lists:nth(rand:uniform(Expression_Length), Expression)
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% defaultCollationClause ::= 'DEFAULT' 'COLLATION' 'USING_NLS_COMP'
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(defaultCollationClause = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            "Default Collation Using_nls_comp"
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% exceptionDeclaration ::= NAME 'EXCEPTION' ';'
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(exceptionDeclaration = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),

    Code =
        [
                lists:nth(rand:uniform(Name_Length), Name) ++ " exception;"
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% expression ::= columnRef
%%              | functionRef
%%              | literal
%%              | NULLX
%%              | parameterRef
%%              | ( '(' expression ')' )
%%              | ( 'NOT' expression )
%%              | ( unaryAddOrSubtract expression )
%%              | ( expression (  'AND' | 'OR' | '+' | '-' | '/' | '*' | '||' | '=' | COMPARISON ) expression )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(expression = Rule) ->
    ?CREATE_CODE_START,
    [{comparison, Comparison}] = ets:lookup(?CODE_TEMPLATES, comparison),
    Comparison_Length = length(Comparison),
    [{expression, Expression}] = ets:lookup(?CODE_TEMPLATES, expression),
    Expression_Length = length(Expression),
    [{unaryAddOrSubtract, UnaryAddOrSubtract}] =
        ets:lookup(?CODE_TEMPLATES, unaryAddOrSubtract),
    UnaryAddOrSubtract_Length = length(UnaryAddOrSubtract),

    Code =
        [
            case rand:uniform(10) rem 10 of
                1 -> lists:append(
                    [
                        "(",
                        lists:nth(rand:uniform(Expression_Length), Expression),
                        ")"
                    ]);
                2 ->
                    "Not " ++ lists:nth(rand:uniform(Expression_Length),
                        Expression);
                3 -> lists:append(
                    [
                        lists:nth(rand:uniform(UnaryAddOrSubtract_Length),
                            UnaryAddOrSubtract),
                        " ",
                        lists:nth(rand:uniform(Expression_Length), Expression)
                    ]);
                4 -> lists:append(
                    [
                        lists:nth(rand:uniform(Expression_Length), Expression),
                        " ",
                        case rand:uniform(6) rem 6 of
                            1 -> "And";
                            2 -> "Or";
                            3 -> "+";
                            4 -> "-";
                            5 -> "/";
                            _ -> "*"
                        end,
                        " ",
                        lists:nth(rand:uniform(Expression_Length), Expression)
                    ]);
                _ -> lists:append(
                    [
                        "(",
                        lists:nth(rand:uniform(Expression_Length), Expression),
                        " ",
                        case rand:uniform(15) rem 15 of
                            1 -> "And";
                            2 -> "Or";
                            3 -> "+";
                            4 -> "-";
                            5 -> "/";
                            6 -> "*";
                            7 -> "=";
                            8 -> "||";
                            _ ->
                                lists:nth(rand:uniform(Comparison_Length),
                                    Comparison)
                        end,
                        " ",
                        lists:nth(rand:uniform(Expression_Length), Expression),
                        ")"
                    ])
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fieldDefinition ::= nameExtended ( dataType_1 | dataType_2 ) ( ( 'NOT' 'NULL' )? default )?
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(fieldDefinition = Rule) ->
    ?CREATE_CODE_START,
    [{dataType_1, DataType_1}] = ets:lookup(?CODE_TEMPLATES, dataType_1),
    DataType_1_Length = length(DataType_1),
    [{dataType_2, DataType_2}] = ets:lookup(?CODE_TEMPLATES, dataType_2),
    DataType_2_Length = length(DataType_2),
    [{default, Default}] = ets:lookup(?CODE_TEMPLATES, default),
    Default_Length = length(Default),
    [{nameExtended, NameExtended}] = ets:lookup(?CODE_TEMPLATES, nameExtended),
    NameExtended_Length = length(NameExtended),

    Code =
        [
            lists:append(
                [
                    lists:nth(rand:uniform(NameExtended_Length), NameExtended),
                    " ",
                    case rand:uniform(5) rem 5 of
                        1 -> lists:nth(rand:uniform(DataType_2_Length),
                            DataType_2);
                        _ -> lists:nth(rand:uniform(DataType_1_Length),
                            DataType_1)
                    end,
                    case rand:uniform(4) rem 4 of
                        1 -> " not null " ++
                        lists:nth(rand:uniform(Default_Length), Default);
                        2 -> lists:nth(rand:uniform(Default_Length), Default);
                        _ -> []
                    end
                ])
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fieldDefinitionCommaList ::= fieldDefinition ( ',' fieldDefinition )*
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(fieldDefinitionCommaList = Rule) ->
    ?CREATE_CODE_START,
    [{fieldDefinition, FieldDefinition}] =
        ets:lookup(?CODE_TEMPLATES, fieldDefinition),
    FieldDefinition_Length = length(FieldDefinition),

    Code =
        [
            case rand:uniform(2) rem 2 of
                1 -> lists:append(
                    [
                        lists:nth(rand:uniform(FieldDefinition_Length),
                            FieldDefinition),
                        ",",
                        lists:nth(rand:uniform(FieldDefinition_Length),
                            FieldDefinition)
                    ]);
                _ -> lists:nth(rand:uniform(FieldDefinition_Length),
                    FieldDefinition)
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% functionAnnotation ::= privilegeAnnotationList
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(functionAnnotation = Rule) ->
    ?CREATE_CODE_START,
    [{apiHiddenAnnotation, ApiHiddenAnnotation}] =
        ets:lookup(?CODE_TEMPLATES, apiHiddenAnnotation),
    ApiHiddenAnnotation_Length = length(ApiHiddenAnnotation),
    [{privilegeAnnotationList, PrivilegeAnnotationList}] =
        ets:lookup(?CODE_TEMPLATES, privilegeAnnotationList),
    PrivilegeAnnotationList_Length = length(PrivilegeAnnotationList),

    Code =
        [
                case rand:uniform(5) rem 5 of
                    1 -> lists:nth(rand:uniform(ApiHiddenAnnotation_Length),
                        ApiHiddenAnnotation);
                    _ -> []
                end
                ++
                lists:nth(rand:uniform(PrivilegeAnnotationList_Length),
                    PrivilegeAnnotationList)
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% functionArg ::= expression
%%               | ( NAME '=>' expression )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(functionArg = Rule) ->
    ?CREATE_CODE_START,
    [{expression, Expression}] = ets:lookup(?CODE_TEMPLATES, expression),
    Expression_Length = length(Expression),
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),

    Code =
        [
                case rand:uniform(2) rem 2 of
                    1 -> lists:nth(rand:uniform(Name_Length), Name) ++ " => ";
                    _ -> []
                end ++
                lists:nth(rand:uniform(Expression_Length), Expression)
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% functionArgCommaList ::= functionArg ( ',' functionArg )*
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(functionArgCommaList = Rule) ->
    ?CREATE_CODE_START,
    [{functionArg, FunctionArg}] = ets:lookup(?CODE_TEMPLATES, functionArg),
    FunctionArg_Length = length(FunctionArg),

    Code =
        [
            case rand:uniform(2) rem 2 of
                1 -> lists:append(
                    [
                        bracket_query_spec(
                            lists:nth(rand:uniform(FunctionArg_Length),
                                FunctionArg)),
                        ",",
                        bracket_query_spec(
                            lists:nth(rand:uniform(FunctionArg_Length),
                                FunctionArg))
                    ]);
                _ ->
                    bracket_query_spec(
                        lists:nth(rand:uniform(FunctionArg_Length),
                            FunctionArg))
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% functionHeading ::= 'FUNCTION' nameExtended ( '(' ( parameterDeclaration )* ')' )?
%%                               'RETURN' dataType 'PIPELINED'?
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(functionHeading = Rule) ->
    ?CREATE_CODE_START,
    [{dataTypeReturn, DataTypeReturn}] =
        ets:lookup(?CODE_TEMPLATES, dataTypeReturn),
    DataTypeReturn_Length = length(DataTypeReturn),
    [{nameExtended, NameExtended}] = ets:lookup(?CODE_TEMPLATES, nameExtended),
    NameExtended_Length = length(NameExtended),
    [{parameterDeclarationCommaList, ParameterDeclarationCommaList}] =
        ets:lookup(?CODE_TEMPLATES, parameterDeclarationCommaList),
    ParameterDeclarationCommaList_Length =
        length(ParameterDeclarationCommaList),

    Code =
        [
            lists:append(
                [
                    "Function ",
                    lists:nth(rand:uniform(NameExtended_Length), NameExtended),
                    case rand:uniform(4) rem 4 of
                        1 -> " ";
                        _ -> lists:append(
                            [
                                "(",
                                lists:nth(
                                    rand:uniform(
                                        ParameterDeclarationCommaList_Length),
                                    ParameterDeclarationCommaList),
                                ")"
                            ])
                    end,
                    "Return ",
                    lists:nth(rand:uniform(DataTypeReturn_Length),
                        DataTypeReturn)
                ])
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% functionRef ::= ( ( NAME '.' )? NAME '.' )? NAME ( '(' functionArg* ')' )?
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(functionRef = Rule) ->
    ?CREATE_CODE_START,
    [{functionArgCommaList, FunctionArgCommaList}] =
        ets:lookup(?CODE_TEMPLATES, functionArgCommaList),
    FunctionArgCommaList_Length = length(FunctionArgCommaList),
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),

    Code =
        [
            case rand:uniform(2) rem 2 of
                1 -> lists:append(
                    [
                        lists:nth(rand:uniform(Name_Length), Name),
                        ".",
                        lists:nth(rand:uniform(Name_Length), Name),
                        " (",
                        lists:nth(rand:uniform(FunctionArgCommaList_Length),
                            FunctionArgCommaList),
                        ")"
                    ]);
                _ -> lists:append(
                    [
                        lists:nth(rand:uniform(Name_Length), Name),
                        " (",
                        lists:nth(rand:uniform(FunctionArgCommaList_Length),
                            FunctionArgCommaList),
                        ")"
                    ])
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    store_code(expression, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ([0-9]+)
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(intnum = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            "0",
            "013456789012345",
            "1",
            "11121",
            "123456",
            "134567890",
            "22345678",
            "23",
            "2345678",
            "2345678901234567",
            "3123456",
            "34567890",
            "411121",
            "456",
            "456789012",
            "5678901234",
            "57891",
            "6456",
            "67890123456",
            "723",
            "789012345678",
            "7891",
            "8901234567890",
            "90",
            "90123456789012"
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% invokerRightsClause ::= 'AUTHID' ( 'CURRENT_USER' | 'DEFINER' )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(invokerRightsClause = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            "Authid Current_user",
            "Authid Definer"
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% literal ::= APPROXNUM
%%           | 'FALSE'
%%           | INTNUM
%%           | STRING
%%           | 'TRUE'
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(literal = Rule) ->
    ?CREATE_CODE_START,
    [{approxnum, Approxnum}] = ets:lookup(?CODE_TEMPLATES, approxnum),
    Approxnum_Length = length(Approxnum),
    [{intnum, Intnum}] = ets:lookup(?CODE_TEMPLATES, intnum),
    Intnum_Length = length(Intnum),
    [{string, String}] = ets:lookup(?CODE_TEMPLATES, string),
    String_Length = length(String),

    Code =
        [
            "False",
            "True"
        ] ++
        [
            case rand:uniform(3) rem 3 of
                1 -> lists:nth(rand:uniform(Approxnum_Length), Approxnum);
                2 -> lists:nth(rand:uniform(Intnum_Length), Intnum);
                _ -> lists:nth(rand:uniform(String_Length), String)

            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    store_code(expression, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (/\*<>([^*]|[\r\n]|(\*+([^*/]|[\r\n])))*\*+/)
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(man_page = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            lists:append([
                ?CHAR_NEWLINE,
                "/*<>",
                ?CHAR_NEWLINE,
                case rand:uniform(2) rem 2 of
                    1 -> lists:append(
                        [
                            "Comment Line No. ",
                            integer_to_list(rand:uniform(?MAX_BASIC)),
                            ?CHAR_NEWLINE,
                            "Comment Line No. ",
                            integer_to_list(rand:uniform(?MAX_BASIC)),
                            ?CHAR_NEWLINE
                        ]);
                    _ -> lists:append([
                        "Comment Line No. ",
                        integer_to_list(rand:uniform(?MAX_BASIC)),
                        ?CHAR_NEWLINE
                    ])
                end,
                "*/",
                ?CHAR_NEWLINE
            ])
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (\"((\$|[^\"]*)*(\"\")*)*\")
%% [A-Za-z][A-Za-z0-9_\$#]*
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(name = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            "\\\"*** ident info ***\\\"",
            "\\\"ident name\\\"",
            "\\\"ident(s)\\\"",
            "\\\"on/off ident\\\"",
            "\\\"X+ident\\\"",
            "credit_limit_ident",
            "credit_limit_ident_1",
            "credit_limit_ident_2",
            "credit_limit_ident_3",
            "credit_limit_ident_4",
            "credit_limit_ident_5",
            "credit_limit_ident__1",
            "credit_limit_ident__2",
            "credit_limit_ident__3",
            "credit_limit_ident__4",
            "credit_limit_ident__5",
            "I#IDENT_000_#",
            "I#IDENT_100_#",
            "I#IDENT_1_",
            "I#IDENT_200_#",
            "I#IDENT_2__",
            "I#IDENT_3_$",
            "I#IDENT_4_$",
            "I#IDENT_5",
            "I#IDENT_6",
            "I#IDENT_7",
            "I#IDENT_8",
            "I#IDENT_9",
            "I1IDENT_000_1",
            "I1IDENT_100_1",
            "I1IDENT_1_",
            "I1IDENT_200_1",
            "I1IDENT_2__",
            "I1IDENT_3_5",
            "I1IDENT_4_6",
            "I1IDENT_5",
            "I1IDENT_6",
            "I1IDENT_7",
            "I1IDENT_8",
            "I1IDENT_9",
            "L#astName_ident",
            "L1astName_ident",
            "m#oney__$tree_ident",
            "m1oney_66tree_ident",
            "o#racle$number_ident",
            "o1racle6number_ident",
            "p#hone$_ident",
            "p1hone5_ident",
            "S#N_$_ident",
            "S1N_5_ident",
            "t#2_ident",
            "t#ry_again__ident",
            "t12_ident",
            "t1ry_again__ident",
            "X#_ident",
            "X#YZ_ident",
            "X1_ident",
            "X1YZ_ident"
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% nameExtended ::= 'API_GROUP' | 'FALSE' | NAME | 'NONE' | 'TRUE'
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(nameExtended = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),

    Code = [
        "api_group",
        "false",
        "none"
        "true"
    ] ++ Name,
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% objectPrivilegeAnnotation ::= '--<>' 'OBJECT_PRIVILEGE' objectPrivilegeType '=' ( NAME '.' )? NAME
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(objectPrivilegeAnnotation = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),
    [{objectPrivilegeType, ObjectPrivilegeType}] =
        ets:lookup(?CODE_TEMPLATES, objectPrivilegeType),
    ObjectPrivilegeType_Length = length(ObjectPrivilegeType),

    Code =
        [
            lists:append(
                [
                    "--<> Object_privilege ",
                    lists:nth(rand:uniform(ObjectPrivilegeType_Length),
                        ObjectPrivilegeType),
                    "=",
                    case rand:uniform(2) rem 2 of
                        1 -> lists:append(
                            [
                                lists:nth(rand:uniform(Name_Length), Name),
                                ".",
                                lists:nth(rand:uniform(Name_Length), Name)
                            ]);
                        _ -> lists:nth(rand:uniform(Name_Length), Name)
                    end,
                    ?CHAR_NEWLINE
                ])
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% objectPrivilegeType ::= 'ALTER'
%%                      | 'DEBUG'
%%                      | 'DELETE'
%%                      | 'EXECUTE'
%%                      | ('FLASHBACK' 'ARCHIVE' )
%%                      | 'INDEX'
%%                      | ( 'INHERIT' 'REMOTE'? 'PRIVILEGES' )
%%                      | 'INSERT'
%%                      | ( 'KEEP' 'SEQUENCE' )
%%                      | ( 'MERGE' 'VIEW' )
%%                      | ( 'ON' 'COMMIT' 'REFRESH' )
%%                      | ( 'QUERY' 'REWRITE' )
%%                      | 'READ'
%%                      | 'REFERENCES'
%%                      | 'SELECT'
%%                      | ( 'TRANSLATE' 'SQL' )
%%                      | 'UNDER'
%%                      | 'UPDATE'
%%                      | 'USE'
%%                      | 'WRITE'
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(objectPrivilegeType = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            "ALTER",
            "DEBUG",
            "DELETE",
            "EXECUTE",
            "FLASHBACK ARCHIVE",
            "INDEX",
            "INHERIT PRIVILEGES",
            "INHERIT REMOTE PRIVILEGES",
            "INSERT",
            "KEEP SEQUENCE",
            "MERGE VIEW",
            "ON COMMIT REFRESH",
            "QUERY REWRITE",
            "READ",
            "REFERENCES",
            "SELECT",
            "TRANSLATE SQL",
            "UNDER",
            "UPDATE",
            "USE",
            "WRITE"
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% packageFunctionDeclaration ::= apiHiddenAnnotation? functionAnnotation? functionHeading packageFunctionDeclarationAttribute* 'MAN_PAGE'? ';'
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(packageFunctionDeclaration = Rule) ->
    ?CREATE_CODE_START,
    [{functionAnnotation, FunctionAnnotation}] = ets:lookup(?CODE_TEMPLATES,
        functionAnnotation),
    FunctionAnnotation_Length = length(FunctionAnnotation),
    [{functionHeading, FunctionHeading}] = ets:lookup(?CODE_TEMPLATES,
        functionHeading),
    FunctionHeading_Length = length(FunctionHeading),
    [{man_page, Man_Page}] = ets:lookup(?CODE_TEMPLATES, man_page),
    Man_Page_Length = length(Man_Page),
    [{packageFunctionDeclarationAttributeList, PackageFunctionDeclarationAttributeList}] =
        ets:lookup(
            ?CODE_TEMPLATES, packageFunctionDeclarationAttributeList),
    PackageFunctionDeclarationAttributeList_Length = length(
        PackageFunctionDeclarationAttributeList),

    Code =
        [
            lists:append(
                [
                    case rand:uniform(2) rem 2 of
                        1 ->
                            lists:nth(rand:uniform(FunctionAnnotation_Length),
                                FunctionAnnotation) ++ " ";
                        _ -> []
                    end,
                        lists:nth(rand:uniform(FunctionHeading_Length),
                            FunctionHeading) ++
                        case rand:uniform(2) rem 2 of
                            1 -> " " ++ lists:nth(rand:uniform(
                                PackageFunctionDeclarationAttributeList_Length),
                                PackageFunctionDeclarationAttributeList);
                            _ -> []
                        end,
                    case rand:uniform(5) rem 5 of
                        1 -> lists:nth(rand:uniform(Man_Page_Length), Man_Page);
                        _ -> []
                    end,
                    ";"
                ])
            || _ <- lists:seq(1, ?MAX_STATEMENT * 2)
        ],
    store_code(Rule, Code, ?MAX_STATEMENT, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% packageFunctionDeclarationAttribute ::= accessibleByClause
%%                                       | 'DETERMINISTIC'
%%                                       | parallelEnabledClause
%%                                       | pipelinedClause
%%                                       | resultCacheClause
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(packageFunctionDeclarationAttribute = Rule) ->
    ?CREATE_CODE_START,
    [{accessibleByClause, AccessibleByClause}] = ets:lookup(?CODE_TEMPLATES,
        accessibleByClause),
    AccessibleByClause_Length = length(AccessibleByClause),
    [{parallelEnabledClause, ParallelEnabledClause}] = ets:lookup(
        ?CODE_TEMPLATES, parallelEnabledClause),
    ParallelEnabledClause_Length = length(ParallelEnabledClause),
    [{pipelinedClause, PipelinedClause}] = ets:lookup(?CODE_TEMPLATES,
        pipelinedClause),
    PipelinedClause_Length = length(PipelinedClause),
    [{resultCacheClause, ResultCacheClause}] = ets:lookup(?CODE_TEMPLATES,
        resultCacheClause),
    ResultCacheClause_Length = length(ResultCacheClause),

    Code =
        [
            case rand:uniform(5) rem 5 of
                1 -> lists:nth(rand:uniform(AccessibleByClause_Length),
                    AccessibleByClause);
                2 -> lists:nth(rand:uniform(ParallelEnabledClause_Length),
                    ParallelEnabledClause);
                3 -> lists:nth(rand:uniform(PipelinedClause_Length),
                    PipelinedClause);
                4 -> lists:nth(rand:uniform(ResultCacheClause_Length),
                    ResultCacheClause);
                _ -> "Deterministic"

            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% packageFunctionDeclarationAttributeList ::= packageFunctionDeclarationAttribute+
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(packageFunctionDeclarationAttributeList = Rule) ->
    ?CREATE_CODE_START,
    [{packageFunctionDeclarationAttribute, PackageFunctionDeclarationAttribute}] =
        ets:lookup(
            ?CODE_TEMPLATES, packageFunctionDeclarationAttribute),
    PackageFunctionDeclarationAttribute_Length = length(
        PackageFunctionDeclarationAttribute),

    Code =
        [
            case rand:uniform(2) rem 2 of
                1 -> lists:append(
                    [
                        lists:nth(rand:uniform(
                            PackageFunctionDeclarationAttribute_Length),
                            PackageFunctionDeclarationAttribute),
                        " ",
                        lists:nth(rand:uniform(
                            PackageFunctionDeclarationAttribute_Length),
                            PackageFunctionDeclarationAttribute)
                    ]);
                _ -> lists:nth(
                    rand:uniform(PackageFunctionDeclarationAttribute_Length),
                    PackageFunctionDeclarationAttribute)
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% packageItem ::= itemDeclaration
%%               | packageFunctionDeclaration
%%               | packageProcedureDeclaration
%%               | typeDefinition
%% -----------------------------------------------------------------------------
%% itemDeclaration ::= constantDeclaration
%%                   | exceptionDeclaration
%% -----------------------------------------------------------------------------
%%  typeDefinition ::= collectionTypeDefinition
%%                   | recordTypeDefinition
%%                   | subtypeDefinition
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(packageItem = Rule) ->
    ?CREATE_CODE_START,
    [{collectionTypeDefinition, CollectionTypeDefinition}] = ets:lookup(
        ?CODE_TEMPLATES, collectionTypeDefinition),
    CollectionTypeDefinition_Length = length(CollectionTypeDefinition),
    [{constantDeclaration, ConstantDeclaration}] = ets:lookup(
        ?CODE_TEMPLATES, constantDeclaration),
    ConstantDeclaration_Length = length(ConstantDeclaration),
    [{exceptionDeclaration, ExceptionDeclaration}] = ets:lookup(
        ?CODE_TEMPLATES, exceptionDeclaration),
    ExceptionDeclaration_Length = length(ExceptionDeclaration),
    [{packageFunctionDeclaration, PackageFunctionDeclaration}] = ets:lookup(
        ?CODE_TEMPLATES, packageFunctionDeclaration),
    PackageFunctionDeclaration_Length = length(PackageFunctionDeclaration),
    [{packageProcedureDeclaration, PackageProcedureDeclaration}] = ets:lookup(
        ?CODE_TEMPLATES, packageProcedureDeclaration),
    PackageProcedureDeclaration_Length = length(PackageProcedureDeclaration),
    [{recordTypeDefinition, RecordTypeDefinition}] = ets:lookup(
        ?CODE_TEMPLATES, recordTypeDefinition),
    RecordTypeDefinition_Length = length(RecordTypeDefinition),
    [{refCursorTypeDefinition, RefCursorTypeDefinition}] = ets:lookup(
        ?CODE_TEMPLATES, refCursorTypeDefinition),
    RefCursorTypeDefinition_Length = length(RefCursorTypeDefinition),
    [{subtypeDefinition, SubtypeDefinition}] =
        ets:lookup(?CODE_TEMPLATES, subtypeDefinition),
    SubtypeDefinition_Length = length(SubtypeDefinition),
    [{variableDeclaration, VariableDeclaration}] = ets:lookup(
        ?CODE_TEMPLATES, variableDeclaration),
    VariableDeclaration_Length = length(VariableDeclaration),

    Code =
        [
            case rand:uniform(20) rem 20 of
                1 -> case rand:uniform(6) rem 6 of
                         1 -> lists:nth(
                             rand:uniform(ConstantDeclaration_Length),
                             ConstantDeclaration);
                         2 -> lists:nth(
                             rand:uniform(ExceptionDeclaration_Length),
                             ExceptionDeclaration);
                         3 -> lists:nth(
                             rand:uniform(RecordTypeDefinition_Length),
                             RecordTypeDefinition);
                         4 -> lists:nth(
                             rand:uniform(RefCursorTypeDefinition_Length),
                             RefCursorTypeDefinition);
                         5 -> case rand:uniform(4) rem 4 of
                                  1 -> lists:nth(
                                      rand:uniform(SubtypeDefinition_Length),
                                      SubtypeDefinition);
                                  _ -> lists:nth(
                                      rand:uniform(
                                          CollectionTypeDefinition_Length),
                                      CollectionTypeDefinition)
                              end;
                         _ -> lists:nth(
                             rand:uniform(VariableDeclaration_Length),
                             VariableDeclaration)
                     end;
                _ -> case rand:uniform(2) rem 2 of
                         1 -> lists:nth(
                             rand:uniform(PackageFunctionDeclaration_Length),
                             PackageFunctionDeclaration);
                         _ -> lists:nth(
                             rand:uniform(PackageProcedureDeclaration_Length),
                             PackageProcedureDeclaration)
                     end
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% packageItemConditional ::= ( ( '$IF' | '$ELSIF' ) expression '$THEN' packageItem '$END'? )
%%                          | ( '$ELSE' packageItem  '$END' )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(packageItemConditional = Rule) ->
    ?CREATE_CODE_START,
    [{expression, Expression}] = ets:lookup(?CODE_TEMPLATES, expression),
    Expression_Length = length(Expression),
    [{packageItem, PackageItem}] = ets:lookup(?CODE_TEMPLATES, packageItem),
    PackageItem_Length = length(PackageItem),

    Code =
        [
            case rand:uniform(3) rem 3 of
                1 ->
                    lists:append(
                        [
                            "$else ",
                            lists:nth(rand:uniform(PackageItem_Length),
                                PackageItem),
                            " $end"
                        ]);
                _ ->
                    lists:append(
                        [
                            case rand:uniform(2) rem 2 of
                                1 -> "$If ";
                                _ -> "$Elsif "
                            end,
                            lists:nth(rand:uniform(Expression_Length),
                                Expression),
                            " $Then ",
                            lists:nth(rand:uniform(PackageItem_Length),
                                PackageItem),
                            case rand:uniform(2) rem 2 of
                                1 -> " $End ";
                                _ -> []
                            end
                        ])
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% packageItemList ::= ( packageItem | packageItemConditional )+
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(packageItemList = Rule) ->
    ?CREATE_CODE_START,
    [{packageItem, PackageItem}] = ets:lookup(?CODE_TEMPLATES, packageItem),
    PackageItem_Length = length(PackageItem),
    [{packageItemConditional, PackageItemConditional}] = ets:lookup(
        ?CODE_TEMPLATES, packageItemConditional),
    PackageItemConditional_Length = length(PackageItemConditional),

    Code =
        [
            case rand:uniform(4) rem 4 of
                1 -> lists:append(
                    [
                        case rand:uniform(2) rem 2 of
                            1 -> lists:nth(rand:uniform(PackageItem_Length),
                                PackageItem);
                            _ ->
                                lists:nth(
                                    rand:uniform(PackageItemConditional_Length),
                                    PackageItemConditional)
                        end,
                        " ",
                        lists:nth(rand:uniform(PackageItem_Length),
                            PackageItem),
                        " ",
                        case rand:uniform(2) rem 2 of
                            1 -> lists:nth(rand:uniform(PackageItem_Length),
                                PackageItem);
                            _ ->
                                lists:nth(
                                    rand:uniform(PackageItemConditional_Length),
                                    PackageItemConditional)
                        end,
                        " ",
                        lists:nth(rand:uniform(PackageItem_Length), PackageItem)
                    ]);
                2 -> lists:append(
                    [
                        case rand:uniform(2) rem 2 of
                            1 -> lists:nth(rand:uniform(PackageItem_Length),
                                PackageItem);
                            _ ->
                                lists:nth(
                                    rand:uniform(PackageItemConditional_Length),
                                    PackageItemConditional)
                        end,
                        " ",
                        lists:nth(rand:uniform(PackageItem_Length),
                            PackageItem),
                        " ",
                        case rand:uniform(2) rem 2 of
                            1 -> lists:nth(rand:uniform(PackageItem_Length),
                                PackageItem);
                            _ ->
                                lists:nth(
                                    rand:uniform(PackageItemConditional_Length),
                                    PackageItemConditional)
                        end
                    ]);
                3 -> lists:append(
                    [
                        lists:nth(rand:uniform(PackageItem_Length),
                            PackageItem),
                        " ",
                        lists:nth(rand:uniform(PackageItem_Length), PackageItem)
                    ]);
                _ -> case rand:uniform(2) rem 2 of
                         1 -> lists:nth(rand:uniform(PackageItem_Length),
                             PackageItem);
                         _ ->
                             lists:nth(
                                 rand:uniform(PackageItemConditional_Length),
                                 PackageItemConditional)
                     end
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% packageProcedureDeclaration ::= apiHiddenAnnotation? procedureAnnotation? procedureHeading accessibleByClause? 'MAN_PAGE'? ';'
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(packageProcedureDeclaration = Rule) ->
    ?CREATE_CODE_START,
    [{accessibleByClause, AccessibleByClause}] = ets:lookup(?CODE_TEMPLATES,
        accessibleByClause),
    AccessibleByClause_Length = length(AccessibleByClause),
    [{procedureAnnotation, ProcedureAnnotation}] = ets:lookup(?CODE_TEMPLATES,
        procedureAnnotation),
    [{man_page, Man_Page}] = ets:lookup(?CODE_TEMPLATES, man_page),
    Man_Page_Length = length(Man_Page),
    ProcedureAnnotation_Length = length(ProcedureAnnotation),
    [{procedureHeading, ProcedureHeading}] = ets:lookup(?CODE_TEMPLATES,
        procedureHeading),
    ProcedureHeading_Length = length(ProcedureHeading),

    Code =
        [
            lists:append(
                [
                    case rand:uniform(2) rem 2 of
                        1 ->
                            lists:nth(rand:uniform(ProcedureAnnotation_Length),
                                ProcedureAnnotation) ++ " ";
                        _ -> []
                    end,
                    lists:nth(rand:uniform(ProcedureHeading_Length),
                        ProcedureHeading),
                    case rand:uniform(2) rem 2 of
                        1 -> " " ++ lists:nth(
                            rand:uniform(AccessibleByClause_Length),
                            AccessibleByClause);
                        _ -> []
                    end,
                    case rand:uniform(5) rem 5 of
                        1 -> lists:nth(rand:uniform(Man_Page_Length), Man_Page);
                        _ -> []
                    end,
                    ";"
                ])
            || _ <- lists:seq(1, ?MAX_STATEMENT * 2)
        ],
    store_code(Rule, Code, ?MAX_STATEMENT, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parallelEnabledClause ::=  'PARALLEL_ENABLED' '(' 'PARTITION' NAME 'BY'
%%                                                 'ANY'
%%                                               | ( ( 'HASH' | 'RANGE' ) '(' columnRef ( ',' columnRef )* ')' streamingClause? )
%%                                               | ( 'VALUE' '(' columnRef ')' )
%%                                               ')'
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(parallelEnabledClause = Rule) ->
    ?CREATE_CODE_START,
    [{columnRef, ColumnRef}] = ets:lookup(?CODE_TEMPLATES, columnRef),
    ColumnRef_Length = length(ColumnRef),
    [{columnRefCommaList, ColumnRefCommaList}] = ets:lookup(?CODE_TEMPLATES,
        columnRefCommaList),
    ColumnRefCommaList_Length = length(ColumnRefCommaList),
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),
    [{streamingClause, StreamingClause}] = ets:lookup(?CODE_TEMPLATES,
        streamingClause),
    StreamingClause_Length = length(StreamingClause),

    Code =
        [
            lists:append(
                [
                    "Parallel_enabled ( Partition ",
                    lists:nth(rand:uniform(Name_Length), Name),
                    " By ",
                    case rand:uniform(5) rem 5 of
                        1 -> "Any";
                        2 -> lists:append(
                            [
                                "Value (",
                                lists:nth(rand:uniform(ColumnRef_Length),
                                    ColumnRef),
                                ")"
                            ]);
                        _ -> lists:append(
                            [
                                case rand:uniform(2) rem 2 of
                                    1 -> "Hash (";
                                    _ -> "Range ("
                                end,
                                lists:nth(
                                    rand:uniform(ColumnRefCommaList_Length),
                                    ColumnRefCommaList),
                                ")",
                                case rand:uniform(2) rem 2 of
                                    1 -> lists:nth(
                                        rand:uniform(StreamingClause_Length),
                                        StreamingClause);
                                    _ -> []
                                end
                            ])
                    end,
                    ")"
                ])
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (\:[A-Za-z0-9_\.][A-Za-z0-9_\.]*)
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(parameter = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            ":.PAR_1",
            ":0par_0",
            ":_par_3",
            ":par_1",
            ":par_1_",
            ":par_2 ",
            ":PAR_2",
            ":par_2_",
            ":PAR_3",
            ":par_3__",
            ":PAR_4",
            ":par_4__",
            ":par_4_",
            ":PAR_5",
            ":par_5_1",
            ":par_5_2_",
            ":PAR_62",
            ":PAR_63",
            ":par_63__",
            ":PAR_64",
            ":par_64__",
            ":par_64_",
            ":PAR_65",
            ":par_65_1",
            ":par_65_2_"
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parameterDeclaration ::= NAME ( 'IN'? ( dataType_1 | dataType_2 ) default? )
%%                             | ( 'IN'? 'OUT' 'NOCOPY'? ( dataType_1 | dataType_2 ) )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(parameterDeclaration = Rule) ->
    ?CREATE_CODE_START,
    [{dataType_1, DataType_1}] = ets:lookup(?CODE_TEMPLATES, dataType_1),
    DataType_1_Length = length(DataType_1),
    [{dataType_2, DataType_2}] = ets:lookup(?CODE_TEMPLATES, dataType_2),
    DataType_2_Length = length(DataType_2),
    [{default, Default}] = ets:lookup(?CODE_TEMPLATES, default),
    Default_Length = length(Default),
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),

    Code =
        [
            lists:append(
                [
                    lists:nth(rand:uniform(Name_Length), Name),
                    case rand:uniform(3) rem 3 of
                        1 -> lists:append(
                            [
                                case rand:uniform(2) rem 2 of
                                    1 -> " In";
                                    _ -> []
                                end,
                                " Out",
                                case rand:uniform(2) rem 2 of
                                    1 -> " Nocopy";
                                    _ -> []
                                end,
                                " ",
                                case rand:uniform(5) rem 5 of
                                    1 -> lists:nth(
                                        rand:uniform(DataType_2_Length),
                                        DataType_2);
                                    _ -> lists:nth(
                                        rand:uniform(DataType_1_Length),
                                        DataType_1)
                                end
                            ]);
                        _ -> lists:append(
                            [
                                case rand:uniform(2) rem 2 of
                                    1 -> " In";
                                    _ -> []
                                end,
                                " ",
                                case rand:uniform(5) rem 5 of
                                    1 -> lists:nth(
                                        rand:uniform(DataType_2_Length),
                                        DataType_2);
                                    _ -> lists:nth(
                                        rand:uniform(DataType_1_Length),
                                        DataType_1)
                                end,
                                case rand:uniform(2) rem 2 of
                                    1 -> lists:nth(rand:uniform(Default_Length),
                                        Default);
                                    _ -> []
                                end
                            ])

                    end
                ])
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parameterDeclarationCommaList ::= parameterDeclaration ( ',' parameterDeclaration )*
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(parameterDeclarationCommaList = Rule) ->
    ?CREATE_CODE_START,
    [{parameterDeclaration, ParameterDeclaration}] = ets:lookup(?CODE_TEMPLATES,
        parameterDeclaration),
    ParameterDeclaration_Length = length(ParameterDeclaration),

    Code =
        [
            case rand:uniform(2) rem 2 of
                1 -> lists:append(
                    [
                        lists:nth(rand:uniform(ParameterDeclaration_Length),
                            ParameterDeclaration),
                        ",",
                        lists:nth(rand:uniform(ParameterDeclaration_Length),
                            ParameterDeclaration)
                    ]);
                _ -> lists:nth(rand:uniform(ParameterDeclaration_Length),
                    ParameterDeclaration)
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parameterRef ::= PARAMETER ( 'INDICATOR'? PARAMETER )?
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(parameterRef = Rule) ->
    ?CREATE_CODE_START,
    [{parameter, Parameter}] = ets:lookup(?CODE_TEMPLATES, parameter),
    Parameter_Length = length(Parameter),

    Code =
        [
                lists:nth(rand:uniform(Parameter_Length), Parameter) ++
                case rand:uniform(3) rem 3 of
                    1 ->
                        " Indicator " ++
                        lists:nth(rand:uniform(Parameter_Length), Parameter);
                    2 ->
                        " " ++
                        lists:nth(rand:uniform(Parameter_Length), Parameter);
                    _ -> []
                end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    store_code(expression, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% pipelinedClause ::= 'PIPELINED' ( ( 'ROW' | 'TABLE' ) 'POLYMORPHIC' )? 'USING' ( NAME '.' )? NAME
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(pipelinedClause = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),

    Code =
        [
            lists:append(
                [
                    "Pipelined",
                    case rand:uniform(3) rem 3 of
                        1 -> " Row Polymorphic";
                        2 -> " Table Polymorphic";
                        _ -> []
                    end,
                    " Using ",
                    case rand:uniform(2) rem 2 of
                        1 -> lists:nth(rand:uniform(Name_Length), Name) ++ ".";
                        _ -> []
                    end,
                    lists:nth(rand:uniform(Name_Length), Name)
                ])
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% plsqlPackageSource ::= ( NAME '.' )? NAME sharingClause? plsqlPackageSourceAttribute*
%%                                      ( 'IS' | 'AS' ) ( packageItem | packageItemConditional )* 'END' ( NAME )?
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(plsqlPackageSource = Rule) ->
    ?CREATE_CODE_START,
    [{man_page, Man_Page}] = ets:lookup(?CODE_TEMPLATES, man_page),
    Man_Page_Length = length(Man_Page),
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),
    [{sharingClause, SharingClause}] = ets:lookup(?CODE_TEMPLATES,
        sharingClause),
    SharingClause_Length = length(SharingClause),
    [{plsqlPackageSourceAttributeList, PlsqlPackageSourceAttributeList}] =
        ets:lookup(
            ?CODE_TEMPLATES, plsqlPackageSourceAttributeList),
    PlsqlPackageSourceAttributeList_Length = length(
        PlsqlPackageSourceAttributeList),
    [{packageItemList, PackageItemList}] = ets:lookup(?CODE_TEMPLATES,
        packageItemList),
    PackageItemList_Length = length(PackageItemList),

    Code =
        [
            lists:append(
                [
                    case rand:uniform(2) rem 2 of
                        1 -> lists:nth(rand:uniform(Name_Length), Name) ++ ".";
                        _ -> []
                    end,
                    lists:nth(rand:uniform(Name_Length), Name),
                    case rand:uniform(2) rem 2 of
                        1 ->
                            " " ++ lists:nth(rand:uniform(SharingClause_Length),
                                SharingClause);
                        _ -> []
                    end,
                    case rand:uniform(2) rem 2 of
                        1 -> " " ++ lists:nth(
                            rand:uniform(
                                PlsqlPackageSourceAttributeList_Length),
                            PlsqlPackageSourceAttributeList);
                        _ -> []
                    end,
                    case rand:uniform(2) rem 2 of
                        1 -> " As ";
                        _ -> " Is "
                    end,
                    case rand:uniform(5) rem 5 of
                        1 -> lists:nth(rand:uniform(Man_Page_Length), Man_Page);
                        _ -> []
                    end,
                    case rand:uniform(5) rem 5 of
                        1 -> lists:nth(rand:uniform(PackageItemList_Length),
                            PackageItemList);
                        _ -> []
                    end,
                    "End",
                    case rand:uniform(2) rem 2 of
                        1 -> " " ++ lists:nth(rand:uniform(Name_Length), Name);
                        _ -> []
                    end
                ])
            || _ <- lists:seq(1, ?MAX_STATEMENT * 2)
        ],
    store_code(Rule, Code, ?MAX_STATEMENT, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% plsqlPackageSourceAttribute ::= accessibleByClause
%%                               | defaultCollationClause
%%                               | invokerRightsClause
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(plsqlPackageSourceAttribute = Rule) ->
    ?CREATE_CODE_START,
    [{accessibleByClause, AccessibleByClause}] = ets:lookup(?CODE_TEMPLATES,
        accessibleByClause),
    AccessibleByClause_Length = length(AccessibleByClause),
    [{defaultCollationClause, DefaultCollationClause}] = ets:lookup(
        ?CODE_TEMPLATES, defaultCollationClause),
    DefaultCollationClause_Length = length(DefaultCollationClause),
    [{invokerRightsClause, InvokerRightsClause}] = ets:lookup(?CODE_TEMPLATES,
        invokerRightsClause),
    InvokerRightsClause_Length = length(InvokerRightsClause),

    Code =
        [
            case rand:uniform(3) rem 3 of
                1 -> lists:nth(rand:uniform(AccessibleByClause_Length),
                    AccessibleByClause);
                2 -> lists:nth(rand:uniform(DefaultCollationClause_Length),
                    DefaultCollationClause);
                _ -> lists:nth(rand:uniform(InvokerRightsClause_Length),
                    InvokerRightsClause)

            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% plsqlPackageSourceAttributeList ::= plsqlPackageSourceAttribute+
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(plsqlPackageSourceAttributeList = Rule) ->
    ?CREATE_CODE_START,
    [{plsqlPackageSourceAttribute, PlsqlPackageSourceAttribute}] = ets:lookup(
        ?CODE_TEMPLATES, plsqlPackageSourceAttribute),
    PlsqlPackageSourceAttribute_Length = length(PlsqlPackageSourceAttribute),

    Code =
        [
            case rand:uniform(2) rem 2 of
                1 -> lists:append(
                    [
                        lists:nth(
                            rand:uniform(PlsqlPackageSourceAttribute_Length),
                            PlsqlPackageSourceAttribute),
                        " ",
                        lists:nth(
                            rand:uniform(PlsqlPackageSourceAttribute_Length),
                            PlsqlPackageSourceAttribute)
                    ]);
                _ -> lists:nth(rand:uniform(PlsqlPackageSourceAttribute_Length),
                    PlsqlPackageSourceAttribute)
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% privilegeAnnotationList ::= ( apiGroupAnnotation | objectPrivilegeAnnotation | systemPrivilegeAnnotation )+
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(privilegeAnnotationList = Rule) ->
    ?CREATE_CODE_START,
    [{apiGroupAnnotation, ApiGroupAnnotation}] =
        ets:lookup(?CODE_TEMPLATES, apiGroupAnnotation),
    ApiGroupAnnotation_Length = length(ApiGroupAnnotation),
    [{objectPrivilegeAnnotation, ObjectPrivilegeAnnotation}] =
        ets:lookup(?CODE_TEMPLATES, objectPrivilegeAnnotation),
    ObjectPrivilegeAnnotation_Length = length(ObjectPrivilegeAnnotation),
    [{systemPrivilegeAnnotation, SystemPrivilegeAnnotation}] =
        ets:lookup(?CODE_TEMPLATES, systemPrivilegeAnnotation),
    SystemPrivilegeAnnotation_Length = length(SystemPrivilegeAnnotation),

    Code =
        [
            case rand:uniform(2) rem 2 of
                1 ->
                    case rand:uniform(3) rem 3 of
                        1 ->
                            lists:nth(rand:uniform(
                                ObjectPrivilegeAnnotation_Length),
                                ObjectPrivilegeAnnotation);
                        2 ->
                            lists:nth(rand:uniform(ApiGroupAnnotation_Length),
                                ApiGroupAnnotation);
                        _ ->
                            lists:nth(
                                rand:uniform(SystemPrivilegeAnnotation_Length),
                                SystemPrivilegeAnnotation)
                    end,
                    case rand:uniform(3) rem 3 of
                        1 ->
                            lists:nth(rand:uniform(
                                ObjectPrivilegeAnnotation_Length),
                                ObjectPrivilegeAnnotation);
                        2 ->
                            lists:nth(rand:uniform(ApiGroupAnnotation_Length),
                                ApiGroupAnnotation);
                        _ ->
                            lists:nth(
                                rand:uniform(SystemPrivilegeAnnotation_Length),
                                SystemPrivilegeAnnotation)
                    end;
                _ ->
                    case rand:uniform(3) rem 3 of
                        1 ->
                            lists:nth(rand:uniform(
                                ObjectPrivilegeAnnotation_Length),
                                ObjectPrivilegeAnnotation);
                        2 ->
                            lists:nth(rand:uniform(ApiGroupAnnotation_Length),
                                ApiGroupAnnotation);
                        _ ->
                            lists:nth(
                                rand:uniform(SystemPrivilegeAnnotation_Length),
                                SystemPrivilegeAnnotation)
                    end
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% procedureAnnotation ::=  privilegeAnnotationList
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(procedureAnnotation = Rule) ->
    ?CREATE_CODE_START,
    [{apiHiddenAnnotation, ApiHiddenAnnotation}] =
        ets:lookup(?CODE_TEMPLATES, apiHiddenAnnotation),
    ApiHiddenAnnotation_Length = length(ApiHiddenAnnotation),
    [{privilegeAnnotationList, PrivilegeAnnotationList}] =
        ets:lookup(?CODE_TEMPLATES, privilegeAnnotationList),
    PrivilegeAnnotationList_Length = length(PrivilegeAnnotationList),

    Code =
        [
                case rand:uniform(5) rem 5 of
                    1 -> lists:nth(rand:uniform(ApiHiddenAnnotation_Length),
                        ApiHiddenAnnotation);
                    _ -> []
                end
                ++
                lists:nth(rand:uniform(PrivilegeAnnotationList_Length),
                    PrivilegeAnnotationList)
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% procedureHeading ::= 'PROCEDURE' NAME ( '(' ( parameterDeclaration )* ')' )?
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(procedureHeading = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),
    [{parameterDeclarationCommaList, ParameterDeclarationCommaList}] =
        ets:lookup(
            ?CODE_TEMPLATES, parameterDeclarationCommaList),
    ParameterDeclarationCommaList_Length = length(
        ParameterDeclarationCommaList),

    Code =
        [
            lists:append(
                [
                    "Procedure ",
                    lists:nth(rand:uniform(Name_Length), Name),
                    case rand:uniform(4) rem 4 of
                        1 -> " ";
                        _ -> lists:append(
                            [
                                "(",
                                lists:nth(
                                    rand:uniform(
                                        ParameterDeclarationCommaList_Length),
                                    ParameterDeclarationCommaList),
                                ")"
                            ])
                    end
                ])
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% recordTypeDefinition ::= 'TYPE' NAME 'IS' 'RECORD' '(' fieldDefinition ( ',' fieldDefinition )* ')' ';'
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(recordTypeDefinition = Rule) ->
    ?CREATE_CODE_START,
    [{fieldDefinitionCommaList, FieldDefinitionCommaList}] =
        ets:lookup(?CODE_TEMPLATES, fieldDefinitionCommaList),
    FieldDefinitionCommaList_Length = length(FieldDefinitionCommaList),
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),

    Code =
        [
            lists:append(
                [
                    "type ",
                    lists:nth(rand:uniform(Name_Length), Name),
                    " is record (",
                    lists:nth(rand:uniform(FieldDefinitionCommaList_Length),
                        FieldDefinitionCommaList),
                    ");"
                ])
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% refCursorTypeDefinition ::= 'TYPE' NAME 'IS' 'REF' 'CURSOR' 'RETURN' ( NAME
%%                                                                 | ( ( NAME '.' )?  NAME ( '%ROWTYPE' | '%TYPE' )? )
%%                                                                 |   ( NAME '.' NAME '.' NAME '%TYPE'? ) )  ';'
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(refCursorTypeDefinition = Rule) ->
    ?CREATE_CODE_START,
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),

    Code =
        [
            lists:append(
                [
                    "Type ",
                    lists:nth(rand:uniform(Name_Length), Name),
                    " Is ref Cursor Return ",
                    case rand:uniform(6) rem 6 of
                        1 -> lists:nth(rand:uniform(Name_Length), Name);
                        2 -> lists:nth(rand:uniform(Name_Length), Name) ++
                        "%Rowtype";
                        3 -> lists:nth(rand:uniform(Name_Length), Name) ++
                        "%Type";
                        4 -> lists:append(
                            [
                                lists:nth(rand:uniform(Name_Length), Name),
                                ".",
                                lists:nth(rand:uniform(Name_Length), Name),
                                "%Rowtype"
                            ]);
                        5 -> lists:append(
                            [
                                lists:nth(rand:uniform(Name_Length), Name),
                                ".",
                                lists:nth(rand:uniform(Name_Length), Name),
                                "%Type"
                            ]);
                        _ -> lists:append(
                            [
                                lists:nth(rand:uniform(Name_Length), Name),
                                ".",
                                lists:nth(rand:uniform(Name_Length), Name),
                                ".",
                                lists:nth(rand:uniform(Name_Length), Name),
                                "%Type"
                            ])
                    end,
                    ";"
                ])
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% resultCacheClause ::= 'RESULT_CACHE' ( 'RELIES_ON' '(' dataSource ( ',' dataSource )* ')' )?
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(resultCacheClause = Rule) ->
    ?CREATE_CODE_START,
    [{dataSourceCommaList, DataSourceCommaList}] = ets:lookup(?CODE_TEMPLATES,
        dataSourceCommaList),
    DataSourceCommaList_Length = length(DataSourceCommaList),

    Code =
        [
                "Result_cache" ++
                case rand:uniform(2) rem 2 of
                    1 -> lists:append(
                        [
                            " Relies_on(",
                            lists:nth(rand:uniform(DataSourceCommaList_Length),
                                DataSourceCommaList),
                            ")"
                        ]);
                    _ -> []
                end
            || _ <- lists:seq(1, ?MAX_BASIC div 10)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sharingClause ::= 'SHARING' '=' ( 'METADATA' | 'NONE' )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(sharingClause = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            "Sharing = Metadata",
            "Sharing = None"
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% osqlplusCommand ::= 'SET' 'DEFINE' ( 'OFF' | 'ON' )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(sqlplusCommand = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            "Set Define Off;",
            "Set Define On;"
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% streamingClause ::= ( 'ORDER' | 'CLUSTER' ) expression 'BY'
%%                                            '(' columnRef ( ',' columnRef )* ')'
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(streamingClause = Rule) ->
    ?CREATE_CODE_START,
    [{columnRefCommaList, ColumnRefCommaList}] = ets:lookup(?CODE_TEMPLATES,
        columnRefCommaList),
    ColumnRefCommaList_Length = length(ColumnRefCommaList),
    [{expression, Expression}] = ets:lookup(?CODE_TEMPLATES, expression),
    Expression_Length = length(Expression),

    Code =
        [
            lists:append(
                [
                    case rand:uniform(2) rem 2 of
                        1 -> "Order ";
                        _ -> "Cluster "
                    end,
                    lists:nth(rand:uniform(Expression_Length), Expression),
                    " By (",
                    lists:nth(rand:uniform(ColumnRefCommaList_Length),
                        ColumnRefCommaList),
                    ")"
                ])
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (\'([^\']*(\'\')*)*\')
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(string = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            "' string_0 '",
            "' string_3'",
            "'STRING''_1'",
            "'string''_2$'",
            "'string_1'",
            "'string_1_'",
            "'string_2 '",
            "'STRING_2'",
            "'string_3$_'",
            "'STRING_3'",
            "'string_4$_'",
            "'STRING_4'",
            "'string_4_'",
            "'STRING_5'",
            "'string_5_1'",
            "'string_5_2_'",
            "'STRING_62'",
            "'string_63$_'",
            "'STRING_63'",
            "'string_64$_'",
            "'STRING_64'",
            "'string_64_'",
            "'STRING_65'",
            "'string_65_1'",
            "'string_65_2_'"
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% subtypeDefinition ::= 'SUBTYPE' NAME 'IS'  ( dataType_1 | dataType_2 )  ( 'NOT' 'NULL' )? ';'
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(subtypeDefinition = Rule) ->
    ?CREATE_CODE_START,
    [{dataType_1, DataType_1}] = ets:lookup(?CODE_TEMPLATES, dataType_1),
    DataType_1_Length = length(DataType_1),
    [{dataType_2, DataType_2}] = ets:lookup(?CODE_TEMPLATES, dataType_2),
    DataType_2_Length = length(DataType_2),
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),

    Code =
        [
            lists:append(
                [
                    "subtype ",
                    lists:nth(rand:uniform(Name_Length), Name),
                    " is ",
                    case rand:uniform(5) rem 5 of
                        1 -> lists:nth(rand:uniform(DataType_2_Length),
                            DataType_2);
                        _ -> lists:nth(rand:uniform(DataType_1_Length),
                            DataType_1)
                    end,
                    case rand:uniform(2) rem 2 of
                        1 -> " not null ";
                        _ -> []
                    end,
                    ";"
                ])
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% systemPrivilegeAnnotation ::= '--<>' 'SYSTEM_PRIVILEGE' '=' systemPrivilegeType
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(systemPrivilegeAnnotation = Rule) ->
    ?CREATE_CODE_START,
    [{systemPrivilegeType, SystemPrivilegeType}] =
        ets:lookup(?CODE_TEMPLATES, systemPrivilegeType),
    SystemPrivilegeType_Length = length(SystemPrivilegeType),

    Code =
        [
            lists:append(
                [
                    "--<> System_privilege = ",
                    lists:nth(rand:uniform(SystemPrivilegeType_Length),
                        SystemPrivilegeType),
                    ?CHAR_NEWLINE
                ])
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% systemPrivilegeType ::= ( 'ALTER' ( ( 'ANY' 'ROLE' ) | 'DATABASE' | 'PROFILE' | 'SESSION' | 'SYSTEM' | 'USER' ) )
%%                      | ( 'CREATE' ( ( 'ANY' ( 'CONTEXT' | 'CREDENTIAL' | 'DIRECTORY' | 'SYNONYM' ) ) |  'CREDENTIAL' | ( 'EXTERNAL' 'JOB' ) | 'JOB? | ''PROCEDURE' | 'PROFILE' | 'ROLE' | 'SEQUENCE' | 'SESSION' | 'TABLE' | 'TRIGGER' | 'TYPE' | 'USER' | 'VIEW' ) )
%%                      | ( 'DROP' ( 'ANY' ( 'DIRECTORY' | 'ROLE') | 'PROFILE' | 'USER' ) )
%%                      | ( 'FLASHBACK' 'ANY' 'TABLE' )
%%                      | ( 'GRANT' 'ANY' ( ( 'OBJECT'? 'PRIVILEGE' ) | 'ROLE' ) )
%%                      | ( 'INHERIT' 'ANY' 'PRIVILEGES' )
%%                      | ( 'SELECT' 'ANY' ( 'DIRECTORY'  | 'TABLE' ) )
%%                      | ( 'SET' 'CONTAINER' )
%%                      | 'SYSDBA'
%%                      | ( 'UNLIMITED' 'TABLESPACE' )
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(systemPrivilegeType = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            "ALTER ANY ROLE",
            "ALTER DATABASE",
            "ALTER PROFILE",
            "ALTER SESSION",
            "ALTER SYSTEM",
            "ALTER USER",
            "CREATE ANY CONTEXT",
            "CREATE ANY CREDENTIAL",
            "CREATE ANY DIRECTORY",
            "CREATE ANY SYNONYM",
            "CREATE CREDENTIAL",
            "CREATE DATABASE LINK",
            "CREATE EXTERNAL JOB",
            "CREATE JOB",
            "CREATE PROCEDURE",
            "CREATE PROFILE",
            "CREATE ROLE",
            "CREATE SEQUENCE",
            "CREATE SESSION",
            "CREATE TABLE",
            "CREATE TRIGGER",
            "CREATE TYPE",
            "CREATE USER",
            "CREATE VIEW",
            "DROP ANY DIRECTORY",
            "DROP ANY PROCEDURE",
            "DROP ANY ROLE",
            "DROP ANY TRIGGER",
            "DROP ANY TYPE",
            "DROP PROFILE",
            "DROP USER",
            "FLASHBACK ANY TABLE",
            "GRANT ANY OBJECT PRIVILEGE",
            "GRANT ANY PRIVILEGE",
            "GRANT ANY ROLE",
            "INHERIT ANY PRIVILEGES",
            "SELECT ANY DIRECTORY",
            "SELECT ANY TABLE",
            "SET CONTAINER",
            "SYSDBA",
            "UNLIMITED TABLESPACE"
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% unaryAddOrSubtract ::= '+' | '-'
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(unaryAddOrSubtract = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            "+",
            "-"
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% unitKind ::= FUNCTION | PACKAGE | PROCEDURE | TRIGGER | TYPE
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(unitKind = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            "Function",
            "Package",
            "Procedure",
            "Trigger",
            "Type"
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% variableDeclaration ::= NAME ( dataType_1 | dataType_2 ) ( 'NOT' 'NULL' )? default? ';'
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(variableDeclaration = Rule) ->
    ?CREATE_CODE_START,
    [{dataType_1, DataType_1}] = ets:lookup(?CODE_TEMPLATES, dataType_1),
    DataType_1_Length = length(DataType_1),
    [{dataType_2, DataType_2}] = ets:lookup(?CODE_TEMPLATES, dataType_2),
    DataType_2_Length = length(DataType_2),
    [{default, Default}] = ets:lookup(?CODE_TEMPLATES, default),
    Default_Length = length(Default),
    [{name, Name}] = ets:lookup(?CODE_TEMPLATES, name),
    Name_Length = length(Name),

    Code =
        [
            lists:append(
                [
                    lists:nth(rand:uniform(Name_Length), Name),
                    " ",
                    case rand:uniform(5) rem 5 of
                        1 -> lists:nth(rand:uniform(DataType_2_Length),
                            DataType_2);
                        _ -> lists:nth(rand:uniform(DataType_1_Length),
                            DataType_1)
                    end,
                    case rand:uniform(2) rem 2 of
                        1 -> " not null ";
                        _ -> []
                    end,
                    case rand:uniform(2) rem 2 of
                        1 -> lists:nth(rand:uniform(Default_Length), Default);
                        _ -> []
                    end,
                    ";"
                ])
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% varrayTypeDef ::= ( ( 'VARYING'? 'ARRAY' ) | 'VARRAY' ) '(' INTNUM ')' 'OF' ( dataType_1 | dataType_2 ) ( 'NOT' 'NULL' )?
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(varrayTypeDef = Rule) ->
    ?CREATE_CODE_START,
    [{dataType_1, DataType_1}] = ets:lookup(?CODE_TEMPLATES, dataType_1),
    DataType_1_Length = length(DataType_1),
    [{dataType_2, DataType_2}] = ets:lookup(?CODE_TEMPLATES, dataType_2),
    DataType_2_Length = length(DataType_2),
    [{intnum, Intnum}] = ets:lookup(?CODE_TEMPLATES, intnum),
    Intnum_Length = length(Intnum),

    Code =
        [
            lists:append(
                [
                    case rand:uniform(5) rem 3 of
                        1 -> "array ";
                        2 -> "varying array ";
                        _ -> "varray "
                    end,
                    "(",
                    lists:nth(rand:uniform(Intnum_Length), Intnum),
                    ") of ",
                    case rand:uniform(5) rem 5 of
                        1 -> lists:nth(rand:uniform(DataType_2_Length),
                            DataType_2);
                        _ -> lists:nth(rand:uniform(DataType_1_Length),
                            DataType_1)
                    end,
                    case rand:uniform(5) rem 5 of
                        1 -> " not null ";
                        _ -> []
                    end
                ])
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Creating Common Test data files.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

file_create_ct_all(_Type, _Complete, _CompactedDetailed, []) ->
    ok;
file_create_ct_all(Type, Complete, CompactedDetailed, [Rule | Rules]) ->
    file_create_ct(Type, Complete, CompactedDetailed, Rule),
    file_create_ct_all(Type, Complete, CompactedDetailed, Rules).

file_create_ct(Type, Complete, CompactedDetailed, Rule) ->
    ?D("Rule: ~s ~n", [atom_to_list(Rule)]),
    [{Rule, Code}] = ets:lookup(?CODE_TEMPLATES, Rule),

    CodeLength = length(Code),
    RuleString = atom_to_list(Rule),

    filelib:ensure_dir(?PATH_CT),

    FileName = lists:append(
        [
            Type,
            "_",
            Complete,
            "_",
            CompactedDetailed,
            "_",
            RuleString,
            "_SUITE"
        ]),
    {ok, File, _} = file:path_open([?PATH_CT], FileName ++ ".erl", [write]),

    erlang:display(io:format(user, "~n" ++ ?MODULE_STRING ++
        " : final common tests ===> ~12.. B file_name: ~s ",
        [CodeLength, FileName ++ ".erl"])),

    {{Current_Year, Current_Month, Current_Day}, _} = calendar:local_time(),

    io:format(File, "~s~n",
        ["%%%-------------------------------------------------------------------"]),
    io:format(File, "~s~n",
        [lists:append(["%%% File        : ", FileName, ".erl"])]),
    io:format(File, "~s~n", [lists:append(
        ["%%% Description : Test Suite for rule: ", RuleString, "."])]),
    io:format(File, "~s~n", ["%%%"]),
    io:format(File, "~s~n", ["%%% Created     : " ++ lists:flatten(
        io_lib:format("~2..0w.~2..0w.~4..0w",
            [Current_Day, Current_Month, Current_Year]))]),
    io:format(File, "~s~n",
        ["%%%-------------------------------------------------------------------"]),
    io:format(File, "~s~n", [lists:append(["-module(", FileName, ")."])]),
    io:format(File, "~s~n", [""]),
    io:format(File, "~s~n", ["-export(["]),
    io:format(File, "~s~n", ["    all/0,"]),
    io:format(File, "~s~n", ["    end_per_suite/1,"]),
    io:format(File, "~s~n", ["    init_per_suite/1,"]),

    case CodeLength of
        0 -> io:format(File, "~s~n", ["    suite/0"]);
        _ -> io:format(File, "~s~n", ["    suite/0,"]),
            case CompactedDetailed of
                "compacted" ->
                    io:format(File, "~s~n",
                        [lists:append(["    test_compacted/1"])]);
                _ -> file_write_ct_export(1, File, CodeLength)
            end
    end,

    io:format(File, "~s~n", ["])."]),
    io:format(File, "~s~n", [""]),
    io:format(File, "~s~n", ["-include_lib(\"common_test/include/ct.hrl\")."]),
    io:format(File, "~s~n", ["-include_lib(\"eunit/include/eunit.hrl\")."]),
    io:format(File, "~s~n", [""]),
    io:format(File, "~s~n",
        ["%%--------------------------------------------------------------------"]),
    io:format(File, "~s~n", ["%% COMMON TEST CALLBACK FUNCTIONS - SUITE"]),
    io:format(File, "~s~n",
        ["%%--------------------------------------------------------------------"]),
    io:format(File, "~s~n", [""]),
    io:format(File, "~s~n", ["suite() ->"]),
    io:format(File, "~s~n", ["    ["]),
    io:format(File, "~s~n", [lists:append(
        ["        {timetrap, {minutes, ", integer_to_list(
            ?TIMETRAP_MINUTES), "}}"])]),
    io:format(File, "~s~n", ["    ]."]),
    io:format(File, "~s~n", [""]),
    io:format(File, "~s~n", ["init_per_suite(Config) ->"]),
    io:format(File, "~s~n", ["    Config."]),
    io:format(File, "~s~n", [""]),
    io:format(File, "~s~n", ["end_per_suite(_Config) ->"]),
    io:format(File, "~s~n", ["    ok."]),
    io:format(File, "~s~n", [""]),
    io:format(File, "~s~n",
        ["%%--------------------------------------------------------------------"]),
    io:format(File, "~s~n", ["%% COMMON TEST CALLBACK FUNCTIONS - ALL"]),
    io:format(File, "~s~n",
        ["%%--------------------------------------------------------------------"]),
    io:format(File, "~s~n", [""]),
    io:format(File, "~s~n", ["all() ->"]),
    io:format(File, "~s~n", ["    ["]),

    case CodeLength of
        0 -> ok;
        _ -> case CompactedDetailed of
                 "compacted" ->
                     io:format(File, "~s~n",
                         [lists:append(["        test_compacted"])]);
                 _ -> file_write_ct_all(1, File, CodeLength)
             end
    end,

    io:format(File, "~s~n", ["    ]."]),
    io:format(File, "~s~n", [""]),
    io:format(File, "~s~n",
        ["%%--------------------------------------------------------------------"]),
    io:format(File, "~s~n", ["%% TEST CASES"]),
    io:format(File, "~s~n",
        ["%%--------------------------------------------------------------------"]),
    io:format(File, "~s~n", [""]),

    case CodeLength of
        0 -> ok;
        _ -> case CompactedDetailed of
                 "compacted" ->
                     io:format(File, "~s~n",
                         [lists:append(["test_compacted(_Config) ->"])]);
                 _ -> ok
             end,
            file_write_ct(1, Type, Complete, CompactedDetailed, File,
                Code)
    end.

file_write_ct(_Current, _Type, _Complete, CompactedDetailed, File, []) ->
    case CompactedDetailed of
        "compacted" -> io:format(File, "~s~n", ["    ok."]);
        _ -> ok
    end,
    file:close(File);
file_write_ct(Current, Type, Complete, CompactedDetailed, File, [H | T]) ->
    case CompactedDetailed of
        "compacted" ->
            case Current rem ?ALIVE_COUNTER of
                0 ->
                    io:format(File, "~s~n", [
                            "    io:format(user, \"Hi Travis CI, I'm still alive - next test is number " ++
                            integer_to_list(Current) ++ " :))~n\", []),"]);
                _ -> []
            end,
            io:format(File, "~s~n", [lists:append([
                "    ",
                case Type of
                    "performance" ->
                        "{ok, _} = plsql_parser:parsetree_with_tokens";
                    _ -> "plsql_parser_test:common_test_source"
                end,
                "(\"",
                H,
                "\"),"
            ])]);
        _ ->
            io:format(File, "~s~n", [lists:append(
                ["test_", integer_to_list(Current), "(_Config) ->"])]),
            io:format(File, "~s~n", [lists:append([
                "    ",
                case Type of
                    "performance" ->
                        "{ok, _} = plsql_parser:parsetree_with_tokens";
                    _ -> "plsql_parser_test:common_test_source"
                end,
                "(\"",
                H,
                "\")."
            ])]),
            io:format(File, "~s~n", [""])
    end,
    file_write_ct(Current + 1, Type, Complete, CompactedDetailed, File,
        T).

file_write_ct_all(Current, File, Target)
    when Current == Target ->
    io:format(File, "~s~n",
        [lists:append(["        test_", integer_to_list(Current)])]);
file_write_ct_all(Current, File, Target) ->
    io:format(File, "~s~n",
        [lists:append(["        test_", integer_to_list(Current), ","])]),
    file_write_ct_all(Current + 1, File, Target).

file_write_ct_export(Current, File, Target)
    when Current == Target ->
    io:format(File, "~s~n",
        [lists:append(["    test_", integer_to_list(Current), "/1"])]);
file_write_ct_export(Current, File, Target) ->
    io:format(File, "~s~n",
        [lists:append(["    test_", integer_to_list(Current), "/1,"])]),
    file_write_ct_export(Current + 1, File, Target).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Creating EUnit data files.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

file_create_eunit_all(_Type, _Complete, []) ->
    ok;
file_create_eunit_all(Type, Complete, [Rule | Rules]) ->
    file_create_eunit(Type, Complete, Rule),
    file_create_eunit_all(Type, Complete, Rules).

file_create_eunit(Type, Complete, Rule) ->
    [{Rule, Code}] = ets:lookup(?CODE_TEMPLATES, Rule),

    RuleStrimg = atom_to_list(Rule),

    filelib:ensure_dir(?PATH_EUNIT),

    FileName =
        lists:append([Type, "_", Complete, "_", RuleStrimg, ".tst"]),
    {ok, File, _} = file:path_open([?PATH_EUNIT], FileName, [write]),

    erlang:display(io:format(user, "~n" ++
        ?MODULE_STRING ++ " : final eunit  tests ===> ~12.. B file_name: ~s~n",
        [length(Code), FileName])),

    io:format(File, "~s~n", ["%%-*- mode: erlang -*-"]),
    io:format(File, "~s~n", ["%%-*- coding: utf-8 -*-"]),
    io:format(File, "~s~n", [""]),
    io:format(File, "~s~n", ["% Manual testing."]),
    io:format(File, "~s~n", ["[{tests, []}]."]),
    io:format(File, "~s~n", [""]),
    io:format(File, "~s~n", ["%%"]),
    io:format(File, "~s~n", ["%% Tests for rule: " ++ RuleStrimg]),
    io:format(File, "~s~n", ["%%"]),
    io:format(File, "~s~n", [""]),

    file_write_eunit(Complete, File, Code).

file_write_eunit(_Complete, File, []) ->
    file:close(File);
file_write_eunit(Complete, File, [H | T]) ->
    io:format(File, "~s~n", [lists:append([
        "\"",
        H,
        "\"."
    ])]),
    file_write_eunit(Complete, File, T).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Store generated code in helper table.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

store_code(Rule, Code, Max, Strict) ->
    case ?LOGGING of
        true ->
            {total_heap_size, MSize} =
                erlang:process_info(whereis(code_server), total_heap_size),
            erlang:display(io:format(user, "~n" ++ ?MODULE_STRING ++
                " : store total_heap   ===> ~12.. B rule: ~s~n",
                [MSize, atom_to_list(Rule)])),
            erlang:display(io:format(user, "~n" ++ ?MODULE_STRING ++
                " : store Code         ===> ~12.. B rule: ~s~n",
                [length(Code), atom_to_list(Rule)]));
        _ -> ok
    end,

    case Max == 0 of
        true ->
            case ?LOGGING of
                true ->
                    erlang:display(io:format(user, "~n" ++ ?MODULE_STRING ++
                        " : store CodeNew      ===> ~12.. B rule: ~s~n",
                        [0, atom_to_list(Rule)]));
                _ -> ok
            end;
        _ ->
            CodeUnique = ordsets:to_list(ordsets:from_list(Code)),
            CodeUnique_Length = length(CodeUnique),
            CodeUniqueSorted = lists:sort(?F_RANDOM, CodeUnique),
            CodeUniqueLimited = case CodeUnique_Length > Max of
                                    true ->
                                        lists:sublist(CodeUniqueSorted, 1, Max);
                                    _ -> CodeUnique
                                end,
            CodeTotal = case ets:lookup(?CODE_TEMPLATES, Rule) of
                            [{Rule, CodeOld}] ->
                                lists:sort(?F_RANDOM, ordsets:to_list(
                                    ordsets:from_list(lists:append(
                                        [CodeOld, CodeUniqueLimited]))));
                            _ -> CodeUniqueLimited
                        end,
            CodeTotal_Length = length(CodeTotal),
            CodeNew = case Strict andalso CodeTotal_Length > Max of
                          true ->
                              [lists:nth(rand:uniform(CodeTotal_Length),
                                  CodeTotal) || _ <- lists:seq(1, Max)];
                          _ -> CodeTotal
                      end,
            ets:insert(?CODE_TEMPLATES, {Rule, CodeNew}),
            case ?LOGGING of
                true ->
                    erlang:display(io:format(user, "~n" ++ ?MODULE_STRING ++
                        " : store CodeNew      ===> ~12.. B rule: ~s~n",
                        [length(CodeNew), atom_to_list(Rule)]));
                _ -> ok
            end
    end,

    case ?LOGGING of
        true ->
            erlang:display(io:format(user, "~n" ++ ?MODULE_STRING ++
                " : store table size   ===> ~12.. B rule: ~s~n",
                [ets:info(?CODE_TEMPLATES, memory), atom_to_list(Rule)]));
        _ -> ok
    end.
