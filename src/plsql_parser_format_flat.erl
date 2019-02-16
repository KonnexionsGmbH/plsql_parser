%% -----------------------------------------------------------------------------
%%
%% plsql_parser_format_flat.erl: PL/SQL - creating a flat version of the PL/SQL
%%                                        script.
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

-module(plsql_parser_format_flat).

-export([
    finalize/2,
    fold/5,
    init/1
]).

-define(NODEBUG, true).

-include("plsql_parser_fold.hrl").

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Setting up optional parameters.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init(Params :: any()) -> [].
init(_Params) ->
    ?D("Start~n Params: ~p~n", [_Params]),
    [].

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Postprocessing of the resulting SQL statement.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec finalize(Params :: any(), Ctx :: string()|tuple()) -> Ctx :: binary()|tuple().
finalize(_Params, Ctx)
    when is_list(Ctx) ->
    ?D("Start~n Params: ~p~n CtxOut: ~p~n", [_Params, Ctx]),
    list_to_binary(string:trim(Ctx)).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Layout methods for processing the various parser subtrees
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% accessibleByClause
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, _PTree, {accessibleByClause, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ " accessible by (";
             _ -> Ctx ++ ")"
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% accessor
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, #{accessor := PTree}, {accessor, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [
                     Ctx,
                     case maps:is_key(unitKind@, PTree) of
                         true ->
                             maps:get(unitKind@, PTree) ++ " ";
                         _ -> []
                     end,
                     maps:get(name@, PTree)
                 ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% accessorCommaList & columnRefCommaList & dataSourceCommaList &
% fieldDefinitionCommaList & functionArgCommaList &
% parameterDeclarationCommaList
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, _PTree, {Rule, Step, Pos} = _FoldState)
    when Rule == accessorCommaList;Rule == columnRefCommaList;
         Rule == dataSourceCommaList;Rule == fieldDefinitionCommaList;
         Rule == functionArgCommaList;Rule == parameterDeclarationCommaList ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case {Step, Pos} of
             {'end', other} -> Ctx ++ ",";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% apiGroupAnnotation
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, #{apiGroupAnnotation := PTree},
    {apiGroupAnnotation, Step} =
        _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [
                     Ctx,
                     "--<> ",
                     maps:get(type@, PTree),
                     " = ",
                     maps:get(apiGroup@, PTree),
                     ?CHAR_NEWLINE
                 ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% apiHiddenAnnotation
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, #{apiHiddenAnnotation := PTree},
    {apiHiddenAnnotation, Step} =
        _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [
                     Ctx,
                     "--<> ",
                     maps:get(type@, PTree),
                     " = ",
                     maps:get(apiHidden@, PTree),
                     ?CHAR_NEWLINE
                 ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% collectionTypeDefinition
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, #{collectionTypeDefinition := _PTree},
    {collectionTypeDefinition, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx;
             _ -> Ctx ++ ";"
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% columnRef
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, #{columnRef := PTree}, {columnRef, Step} = _FoldState)
    when is_list(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ PTree;
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% columnRefCommaList@
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, _PTree, {columnRefCommaList@, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ "(";
             _ -> Ctx ++ ")"
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% constantDeclaration
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, #{constantDeclaration := _PTree},
    {constantDeclaration, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx;
             _ -> Ctx ++ ";"
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% constantName
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, #{constantName := PTree}, {constantName, Step} =
    _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [
                     Ctx,
                     PTree,
                     " constant"
                 ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% createPackage
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, #{createPackage := PTree}, {createPackage, Step} =
    _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [
                     Ctx,
                     "create",
                     case maps:is_key(orReplace@, PTree) of
                         true -> " " ++ maps:get(orReplace@, PTree);
                         _ -> []
                     end,
                     case maps:is_key(editionable@, PTree) of
                         true -> " " ++ maps:get(editionable@, PTree);
                         _ -> []
                     end,
                     " package"
                 ]);
             _ -> lists:append(
                 [
                     Ctx,
                     ";",
                     case maps:is_key(slash@, PTree) of
                         true ->
                             "/";
                         _ -> []
                     end
                 ])
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% dataSource
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, #{dataSource := PTree}, {dataSource, Step} =
    _FoldState)
    when is_list(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ PTree;
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% dataSourceCommaList@
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, _PTree, {dataSourceCommaList@, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ "(";
             _ -> Ctx ++ ")"
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% dataType
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, #{dataType := PTree}, {dataType, Step} = _FoldState)
    when is_map(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    Type = maps:get(type@, PTree),
    RT = case Step of
             start -> lists:append(
                 [
                     Ctx,
                     " ",
                     Type,
                     case Type of
                         "INTERVAL DAY" -> lists:append(
                             [
                                 case maps:is_key(dayPrecision@, PTree) of
                                     true -> lists:append(
                                         [
                                             "(",
                                             maps:get(dayPrecision@, PTree),
                                             ")"
                                         ]);
                                     _ -> []
                                 end,
                                 " to second",
                                 case maps:is_key(secondPrecision@, PTree) of
                                     true -> lists:append(
                                         [
                                             "(",
                                             maps:get(secondPrecision@, PTree),
                                             ")"
                                         ]);
                                     _ -> []
                                 end
                             ]);
                         "INTERVAL YEAR" -> lists:append(
                             [
                                 case maps:is_key(precision@, PTree) of
                                     true -> lists:append(
                                         [
                                             "(",
                                             maps:get(precision@, PTree),
                                             ")"
                                         ]);
                                     _ -> []
                                 end,
                                 " to month"
                             ]);
                         "TIMESTAMP" -> lists:append(
                             [
                                 case maps:is_key(precision@, PTree) of
                                     true -> lists:append(
                                         [
                                             "(",
                                             maps:get(precision@, PTree),
                                             ")"
                                         ]);
                                     _ -> []
                                 end,
                                 case maps:is_key(timeZone@, PTree) of
                                     true -> lists:append(
                                         [
                                             " with",
                                             case maps:is_key(local@, PTree) of
                                                 true -> " local";
                                                 _ -> []
                                             end,
                                             " time zone"
                                         ]);
                                     _ -> []
                                 end
                             ]);
                         _ -> lists:append(
                             [
                                 case maps:is_key(precision@, PTree) of
                                     true -> lists:append(
                                         [
                                             "(",
                                             maps:get(precision@, PTree),
                                             case maps:is_key(scale@, PTree) of
                                                 true -> lists:append(
                                                     [
                                                         ",",
                                                         maps:get(scale@,
                                                             PTree),
                                                         ")"
                                                     ]);
                                                 _ -> ")"
                                             end
                                         ]);
                                     _ -> []
                                 end,
                                 case maps:is_key(size@, PTree) of
                                     true -> lists:append(
                                         [
                                             "(",
                                             maps:get(size@, PTree),
                                             case maps:is_key(sizeType@,
                                                 PTree) of
                                                 true -> lists:append(
                                                     [
                                                         " ",
                                                         maps:get(sizeType@,
                                                             PTree),
                                                         ")"
                                                     ]);
                                                 _ -> ")"
                                             end
                                         ]);
                                     _ -> []
                                 end,
                                 case maps:is_key(attribute@, PTree) of
                                     true -> maps:get(attribute@, PTree);
                                     _ -> []
                                 end
                             ])
                     end
                 ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% dataTypeIndex
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, _PTree, {dataTypeIndex, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start ->
                 Ctx ++ " index by ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% dataTypeTable
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, _PTree, {dataTypeTable, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start ->
                 Ctx ++ "table of ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% defaultCollationClause
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, #{defaultCollationClause := PTree},
    {defaultCollationClause, Step} = _FoldState)
    when is_list(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [
                     Ctx,
                     " ",
                     PTree
                 ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% default
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, #{default := PTree}, {default, Step} = _FoldState)
    when is_map(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [
                     Ctx,
                     " ",
                     maps:get(type@, PTree)
                 ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% defaultValue@_@
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, _PTree, {defaultValue@_@, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ " ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% eceptionDeclaration
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, #{exceptionDeclaration := PTree},
    {exceptionDeclaration, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> Ctx;
             _ -> lists:append(
                 [
                     Ctx,
                     PTree,
                     " exception;"
                 ])
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% expression
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, #{expression := PTree}, {expression, Step} =
    _FoldState)
    when is_atom(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [
                     Ctx,
                     atom_to_list(PTree)
                 ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, #{expression := PTree}, {expression, Step} =
    _FoldState)
    when is_map(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    Operator = case maps:is_key(operator@, PTree) of
                   true -> maps:get(operator@, PTree);
                   _ -> []
               end,
    RT = case Step of
             start -> Ctx ++ case Operator of
                                 'NOT' -> "not ";
                                 '(' -> "(";
                                 _ -> []
                             end;
             _ -> Ctx ++ case Operator of
                             '(' -> ")";
                             _ -> []
                         end
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% fieldDefinition
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, #{fieldDefinition := PTree},
    {fieldDefinition, Step} = _FoldState)
    when is_map(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ maps:get(name@, PTree);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% functionArg
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, #{functionArg := PTree}, {functionArg, Step} =
    _FoldState)
    when map_size(PTree) == 2 ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [
                     Ctx,
                     maps:get(name@, PTree),
                     "=>"
                 ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% functionArgCommaList@
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, _PTree, {functionArgCommaList@, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ "(";
             _ -> Ctx ++ ")"
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% functionHeading
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, #{functionHeading := PTree}, {functionHeading, Step} =
    _FoldState)
    when is_map(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [
                     Ctx,
                     "function ",
                     maps:get(name@, PTree)
                 ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% functionRef
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, #{functionRef := PTree}, {functionRef, Step} =
    _FoldState)
    when is_map(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ maps:get(name@, PTree);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% invokerRightsClause
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, #{invokerRightsClause := PTree},
    {invokerRightsClause, Step} = _FoldState)
    when is_list(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [
                     Ctx,
                     " authid ",
                     PTree
                 ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% literal
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, #{literal := PTree}, {literal, Step} = _FoldState)
    when is_list(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [
                     Ctx,
                     PTree
                 ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% notNull
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, #{notNull := _PTree}, {notNull, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ " not null ";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% objectPrivilegeAnnotation
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, #{objectPrivilegeAnnotation := PTree},
    {objectPrivilegeAnnotation, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [
                     Ctx,
                     "--<> ",
                     maps:get(type@, PTree),
                     " ",
                     maps:get(privilegeType@, PTree),
                     " = ",
                     maps:get(object@, PTree),
                     ?CHAR_NEWLINE
                 ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% operator
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, PTree, {operator, Step} = _FoldState)
    when is_atom(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [
                     Ctx,
                     " ",
                     atom_to_list(PTree),
                     " "
                 ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% packageFunctionDeclaration
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, #{packageFunctionDeclaration := PTree},
    {packageFunctionDeclaration, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> Ctx;
             _ -> lists:append(
                 [
                     Ctx,
                     case maps:is_key(man_page@, PTree) of
                         true -> maps:get(man_page@, PTree);
                         _ -> []
                     end,
                     ";"
                 ])
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% packageFunctionDeclarationAttribute
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, #{packageFunctionDeclarationAttribute := PTree},
    {packageFunctionDeclarationAttribute, Step} = _FoldState)
    when is_list(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [
                     Ctx,
                     " ",
                     PTree
                 ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% packageItemConditional
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, #{packageItemConditional := PTree},
    {packageItemConditional, Step} = _FoldState)
    when is_map(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [
                     Ctx,
                     maps:get(start@, PTree),
                     " "
                 ]);
             _ -> lists:append(
                 [
                     Ctx,
                     " ",
                     case maps:is_key(end@, PTree) of
                         true -> maps:get(end@, PTree) ++ " ";
                         _ -> []
                     end
                 ])
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% packageItemList@_@
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, #{packageItemList@_@ := PTree},
    {packageItemList@_@, Step} = _FoldState)
    when is_map(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start ->
                 lists:append(
                     [
                         Ctx,
                         " ",
                         maps:get(asIs@, PTree),
                         case maps:is_key(man_page@, PTree) of
                             true -> " " ++ maps:get(man_page@, PTree);
                             _ -> []
                         end,
                         " "
                     ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% packageProcedureDeclaration
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, #{packageProcedureDeclaration := PTree},
    {packageProcedureDeclaration, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> Ctx;
             _ -> lists:append(
                 [
                     Ctx,
                     case maps:is_key(man_page@, PTree) of
                         true -> maps:get(man_page@, PTree);
                         _ -> []
                     end,
                     ";"
                 ])
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% parallelEnabledClause
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, #{parallelEnabledClause := PTree},
    {parallelEnabledClause, Step} = _FoldState)
    when is_map(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [
                     Ctx,
                     " parallel_enabled(partition ",
                     maps:get(name@, PTree),
                     " by ",
                     maps:get(type@, PTree)
                 ]);
             _ -> Ctx ++ ")"
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% parameterDeclaration
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, #{parameterDeclaration := PTree},
    {parameterDeclaration, Step} = _FoldState)
    when is_map(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [
                     Ctx,
                     maps:get(name@, PTree),
                     case maps:is_key(mode@, PTree) of
                         true -> " " ++ maps:get(mode@, PTree);
                         _ -> []
                     end,
                     case maps:is_key(nocopy@, PTree) of
                         true -> " " ++ maps:get(nocopy@, PTree);
                         _ -> []
                     end
                 ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% parameterDeclarationCommaList@
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, _PTree, {parameterDeclarationCommaList@, Step} =
    _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ "(";
             _ -> Ctx ++ ")"
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% parameterRef
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, #{parameterRef := PTree}, {parameterRef, Step} =
    _FoldState)
    when is_list(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ PTree;
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);
fold([], _FunState, Ctx, #{parameterRef := PTree}, {parameterRef, Step} =
    _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [
                     Ctx,
                     maps:get(parameterLeft@, PTree),
                     case maps:is_key(indicator@, PTree) of
                         true -> " indicator ";
                         _ -> " "
                     end,
                     maps:get(parameterRight@, PTree)
                 ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% pipelinedClause
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, #{pipelinedClause := PTree}, {pipelinedClause, Step} =
    _FoldState)
    when is_map(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [
                     Ctx,
                     " pipelined ",
                     case maps:is_key(type@, PTree) of
                         true -> maps:get(type@, PTree) ++ " polymorphic ";
                         _ -> []
                     end,
                     case maps:is_key(implementationPackage@, PTree) of
                         true -> " using " ++
                         maps:get(implementationPackage@, PTree);
                         _ -> []
                     end
                 ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% plsqlPackageSource
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, #{plsqlPackageSource := PTree},
    {plsqlPackageSource, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [
                     Ctx,
                     " ",
                     maps:get(packageNameStart@, PTree)
                 ]);
             _ -> lists:append(
                 [
                     Ctx,
                     "end",
                     case maps:is_key(packageNameEnd@, PTree) of
                         true -> " " ++ maps:get(packageNameEnd@, PTree);
                         _ -> []
                     end
                 ])
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% procedureHeading
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, #{procedureHeading := PTree},
    {procedureHeading, Step} = _FoldState)
    when is_map(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [
                     Ctx,
                     "procedure ",
                     maps:get(name@, PTree)
                 ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% recordTypeDefinition
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, #{recordTypeDefinition := _PTree},
    {recordTypeDefinition, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx;
             _ -> Ctx ++ ");"
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% recordTypeName
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, #{recordTypeName := PTree}, {recordTypeName, Step} =
    _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [
                     Ctx,
                     "type ",
                     PTree,
                     " is record ("
                 ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% resultCacheClause
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, #{resultCacheClause := PTree},
    {resultCacheClause, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [
                     Ctx,
                     " result_cache",
                     case maps:get(dataSourceCommaList@, PTree) of
                         {} -> [];
                         _ -> " relies_on"
                     end
                 ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% return
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, _PTree, {return, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ " return";
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sharingClause
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, #{sharingClause := PTree}, {sharingClause, Step} =
    _FoldState)
    when is_list(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [
                     Ctx,
                     " sharing = ",
                     PTree
                 ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% streamingClause
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, #{streamingClause := PTree}, {streamingClause, Step} =
    _FoldState)
    when is_map(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [
                     Ctx,
                     maps:get(type@, PTree),
                     " "
                 ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% streamingClauseExpression@_@
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, _PTree, {streamingClauseExpression@_@, Step} =
    _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx;
             _ -> Ctx ++ " by "
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% subtypeDefinition
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, #{subtypeDefinition := _PTree},
    {subtypeDefinition, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx;
             _ -> Ctx ++ ";"
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% subtypeName
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, #{subtypeName := PTree}, {subtypeName, Step} =
    _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [
                     Ctx,
                     "subtype ",
                     PTree,
                     " is"
                 ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% systemPrivilegeAnnotation
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, #{systemPrivilegeAnnotation := PTree},
    {systemPrivilegeAnnotation, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [
                     Ctx,
                     "--<> ",
                     maps:get(type@, PTree),
                     " = ",
                     maps:get(privilegeType@, PTree),
                     ?CHAR_NEWLINE
                 ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% thenExpression@_@
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, _PTree, {thenExpression@_@, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, _PTree, _FoldState),
    RT = case Step of
             start -> Ctx;
             _ -> Ctx ++ " $then "
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% typeName
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, #{typeName := PTree}, {typeName, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append(
                 [
                     Ctx,
                     "type ",
                     PTree,
                     " is "
                 ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% unaryAddOrSubtract
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, #{unaryAddOrSubtract := PTree},
    {unaryAddOrSubtract, Step} = _FoldState)
    when is_list(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ PTree;
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% varraySize
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, #{varraySize := PTree},
    {varraySize, Step} = _FoldState)
    when is_list(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> lists:append([Ctx,
                 "(",
                 PTree,
                 ") of"
             ]);
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% varrayType
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, #{varrayType := PTree},
    {varrayType, Step} = _FoldState)
    when is_list(PTree) ->
    ?CUSTOM_INIT(_FunState, Ctx, PTree, _FoldState),
    RT = case Step of
             start -> Ctx ++ PTree;
             _ -> Ctx
         end,
    ?CUSTOM_RESULT(RT);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NO ACTION.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, Ctx, _PTree, {Rule, _Step, _Pos}) when
    Rule == packageFunctionDeclarationAttributeList;
    Rule == packageItemList;
    Rule == plsqlPackageSourceAttributeList;
    Rule == plsqlUnit;
    Rule == privilegeAnnotationList ->
    Ctx;

fold([], _FunState, Ctx, _PTree, {Rule, _Step}) when
    Rule == assocArrayTypeDef;
    Rule == columnRefCommaList;
    Rule == conditionalExpression;
    Rule == dataSourceCommaList;
    Rule == expression;
    Rule == functionAnnotation;
    Rule == functionArg;
    Rule == functionArgCommaList;
    Rule == packageFunctionDeclaration@_@;
    Rule == packageFunctionDeclarationAttribute;
    Rule == packageItemSimple;
    Rule == packageProcedureDeclaration@_@;
    Rule == parameterDeclarationCommaList;
    Rule == plsqlPackageSourceAttribute;
    Rule == plsqlUnit;
    Rule == plsqlUnitList;
    Rule == privilegeAnnotationList@_@;
    Rule == procedureAnnotation;
    Rule == scalarExpression;
    Rule == scalarSubExpression;
    Rule == varrayTypeDef ->
    Ctx;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% UNSUPPORTED
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold([], _FunState, _Ctx, PTree, {Rule, Step, Pos} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, _Ctx, PTree, _FoldState),
    throw({lists:append([
        "[",
        ?MODULE_STRING,
        ":",
        atom_to_list(?FUNCTION_NAME),
        "] error parser subtree not supported [rule=",
        atom_to_list(Rule),
        " / step=",
        atom_to_list(Step),
        " / pos=",
        atom_to_list(Pos),
        "]"
    ]), PTree});
fold([], _FunState, _Ctx, PTree, {Rule, Step} = _FoldState) ->
    ?CUSTOM_INIT(_FunState, _Ctx, PTree, _FoldState),
    throw({lists:append([
        "[",
        ?MODULE_STRING,
        ":",
        atom_to_list(?FUNCTION_NAME),
        "] error parser subtree not supported [rule=",
        atom_to_list(Rule),
        " / step=",
        atom_to_list(Step),
        "]"
    ]), PTree}).
