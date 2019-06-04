%% -----------------------------------------------------------------------------
%%
%% plsql_parser_fold.erl: PL/SQL - unparsing utilities.
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

-module(plsql_parser_fold).

-export([
    fold/5,
    get_stmnt_clause_curr/1,
    top_down/3
]).

-define(NODEBUG, true).

-include("plsql_lexer.hrl").
-include("plsql_parser_fold.hrl").

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% top-down processing.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec top_down(Module :: atom(), PLSQLParseTree :: list(), Params :: any()) -> binary()|tuple().
top_down(Module, PLSQLParseTree, Params) ->
    ?D("Start~n Module: ~p~n SQL: ~p~n Params: ~p~n",
        [Module, PLSQLParseTree, Params]),
    fold_state_common(Module, PLSQLParseTree, Params).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% common processing.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec fold_state_common(Module :: atom(), PLSQLParseTree :: list(), Params :: any()) -> binary()|tuple().
fold_state_common(Module, PLSQLParseTree, Params) ->
    ParseTree = case PLSQLParseTree of
                    [PT | _] when is_map(PT) -> lists:flatten(PLSQLParseTree, []);
                    _ -> {ok, PT} = plsql_parser:parsetree(PLSQLParseTree),
                        lists:flatten(PT, [])
                end,
    ParamsInitialized = Module:init(Params),
    {ok, Sql} = fold(fun Module:fold/5, ParamsInitialized, #fstate{}, ParseTree,
        []),
    Module:finalize(ParamsInitialized, Sql).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Folder starting method.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec fold(Fun :: fun(), LOpts :: term(), FunState :: tuple(), PTree :: list(), Ctx :: term()) ->
    Ctx :: term().
fold(Fun, LOpts, FunState, PTree, CtxIn) ->
    ?D("Start~n LOpts: ~p~n FunState: ~p~n PTree: ~p~n CtxIn: ~p~n",
        [LOpts, FunState, PTree, CtxIn]),
    RT = fold_i(Fun, LOpts, FunState, CtxIn, PTree),
    ?D("~n CtxOut: ~p~n", [RT]),
    {ok, RT}.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Folder methods for processing the various parser subtrees
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% accessibleByClause
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{accessibleByClause := Value} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = accessibleByClause,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtx1 = list_elem_ext_rule(Fun, LOpts, FunState, NewCtxS,
        accessorCommaList, maps:get(accessorCommaList@, Value)),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% accessor
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{accessor := _Value} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = accessor,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% accessorCommaList & columnRefCommaList & dataSourceCommaList &
% fieldDefinitionCommaList, functionArgCommaList & packageItemList &
% parameterDeclarationCommaList & plsqlPackageSourceAttributeList & plsqlUnit &
% privilegeAnnotationList & restrictReferencesList
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, {Rule, Pos, PTree})
    when Rule == accessorCommaList;Rule == columnRefCommaList;
         Rule == dataSourceCommaList;Rule == fieldDefinitionCommaList;
         Rule == functionArgCommaList;Rule == packageItemList;
         Rule == parameterDeclarationCommaList;
         Rule == plsqlPackageSourceAttributeList;Rule == plsqlUnit;
         Rule == privilegeAnnotationList;Rule == restrictReferencesList ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start, Pos}),
    NewCtx1 = fold_i(Fun, LOpts, FunState, NewCtxS, PTree),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree, {Rule, 'end', Pos}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% apiGroupAnnotation
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{apiGroupAnnotation := _Value} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = apiGroupAnnotation,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% apiHiddenAnnotation
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{apiHiddenAnnotation := _Value} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = apiHiddenAnnotation,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% assocArrayTypeDef
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{assocArrayTypeDef := Value} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = assocArrayTypeDef,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtx1 =
        fold_i(Fun, LOpts, FunState, NewCtxS, maps:get(dataTypeTable@, Value)),
    NewCtx2 = case maps:is_key(notNull@,
        Value) of
                  true ->
                      fold_i(Fun, LOpts, FunState, NewCtx1,
                          maps:get(notNull@, Value));
                  _ -> NewCtx1
              end,
    NewCtx3 = case maps:is_key(dataTypeIndex@, Value) of
                  true ->
                      fold_i(Fun, LOpts, FunState, NewCtx2,
                          maps:get(dataTypeIndex@, Value));
                  _ -> NewCtx2
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx3, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% collectionTypeDefinitionA
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunStateIn, Ctx, #{collectionTypeDefinition := Value} =
    PTree) ->
    Rule = collectionTypeDefinition,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtx1 = fold_i(Fun, LOpts, FunState, NewCtxS, maps:get(typeName@, Value)),
    NewCtx2 = case maps:is_key(assocArrayTypeDef@, Value) of
                  true ->
                      fold_i(Fun, LOpts, FunState, NewCtx1,
                          maps:get(assocArrayTypeDef@, Value));
                  _ -> fold_i(Fun, LOpts, FunState, NewCtx1,
                      maps:get(varrayTypeDef@, Value))
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx2, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% columnRef
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{columnRef := Value} = PTree)
    when is_list(Value) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = columnRef,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% columnRefCommaList@
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, {columnRefCommaList@, PTree})
    when is_list(PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = columnRefCommaList@,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtx1 = list_elem_ext_rule(Fun, LOpts, FunState, NewCtxS,
        columnRefCommaList, PTree),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% constantDeclaration
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunStateIn, Ctx, #{constantDeclaration := Value} = PTree) ->
    Rule = constantDeclaration,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtx1 =
        fold_i(Fun, LOpts, FunState, NewCtxS, maps:get(constantName@, Value)),
    NewCtx2 = fold_i(Fun, LOpts, FunState, NewCtx1, maps:get(dataType@, Value)),
    NewCtx3 = case maps:is_key(notNull@, Value) of
                  true ->
                      fold_i(Fun, LOpts, FunState, NewCtx2,
                          maps:get(notNull@, Value));
                  _ -> NewCtx2
              end,
    NewCtx4 = fold_i(Fun, LOpts, FunState, NewCtx3, maps:get(default@, Value)),
    NewCtxE = Fun(LOpts, FunState, NewCtx4, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% constantName
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{constantName := _Value} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = constantName,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% createPackage
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{createPackage := Value} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = createPackage,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtx1 = fold_i(Fun, LOpts, FunState, NewCtxS,
        maps:get(plsqlPackageSource@, Value)),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% dataSource
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{dataSource := _Value} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = dataSource,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% dataSourceCommaList@
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, {dataSourceCommaList@, PTree})
    when is_list(PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = dataSourceCommaList@,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtx1 = list_elem_ext_rule(Fun, LOpts, FunState, NewCtxS,
        dataSourceCommaList, PTree),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);
fold_i(Fun, LOpts, FunState, Ctx, {dataSourceCommaList@, PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = dataSourceCommaList,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% dataType
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{dataType := _Value} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = dataType,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% dataTypeIndex
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{dataTypeIndex := _Value} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = dataTypeIndex,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtx1 = fold_i(Fun, LOpts, FunState, NewCtxS,
        maps:get(dataType@, PTree)),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% dataTypeTable
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{dataTypeTable := _Value} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = dataTypeTable,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtx1 = fold_i(Fun, LOpts, FunState, NewCtxS,
        maps:get(dataType@, PTree)),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% default
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{default := Value} = PTree)
    when is_map(Value) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = default,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtx1 = fold_i(Fun, LOpts, FunState, NewCtxS,
        #{defaultValue@_@ => maps:get(value@, Value)}),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% defaultCollationClause
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{defaultCollationClause := _Value} =
    PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = defaultCollationClause,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% defaultValue@_@
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{defaultValue@_@ := Value} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = defaultValue@_@,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtx1 = fold_i(Fun, LOpts, FunState, NewCtxS, Value),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% exceptionDeclaration
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunStateIn, Ctx, #{exceptionDeclaration := _Value} =
    PTree) ->
    Rule = exceptionDeclaration,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% expression
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{expression := Value} = PTree)
    when is_atom(Value);is_list(Value) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = expression,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);
fold_i(Fun, LOpts, FunState, Ctx, #{expression := Value} = PTree)
    when map_size(Value) == 1 ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = expression,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtx1 = fold_i(Fun, LOpts, FunState, NewCtxS, Value),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);
fold_i(Fun, LOpts, FunState, Ctx, #{expression := Value} = PTree)
    when map_size(Value) == 2 ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = expression,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    Operator = maps:get(operator@, Value),
    NewCtx1 = case is_map(Operator) of
                  true ->
                      fold_i(Fun, LOpts, FunState, NewCtxS,
                          maps:get(operator@, Value));
                  _ -> NewCtxS
              end,
    NewCtx2 = fold_i(Fun, LOpts, FunState, NewCtx1,
        maps:get(expression@, Value)),
    NewCtxE = Fun(LOpts, FunState, NewCtx2, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);
fold_i(Fun, LOpts, FunState, Ctx, #{expression := Value} = PTree)
    when is_map(Value) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = expression,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtx1 = fold_i(Fun, LOpts, FunState, NewCtxS,
        maps:get(expressionLeft@, Value)),
    NewCtx2 = fold_i(Fun, LOpts, FunState, NewCtx1,
        {operator, maps:get(operator@, Value)}),
    NewCtx3 = fold_i(Fun, LOpts, FunState, NewCtx2,
        maps:get(expressionRight@, Value)),
    NewCtxE = Fun(LOpts, FunState, NewCtx3, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% fieldDefinition
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{fieldDefinition := Value} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = fieldDefinition,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtx1 = fold_i(Fun, LOpts, FunState, NewCtxS, maps:get(dataType@, Value)),
    NewCtx2 = case maps:is_key(notNull@, Value) of
                  true ->
                      fold_i(Fun, LOpts, FunState, NewCtx1,
                          maps:get(notNull@, Value));
                  _ -> NewCtx1
              end,
    NewCtx3 = case maps:is_key(default@, Value) of
                  true ->
                      fold_i(Fun, LOpts, FunState, NewCtx2,
                          maps:get(default@, Value));
                  _ -> NewCtx2
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx3, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% functionAnnotation
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{functionAnnotation := Value} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = functionAnnotation,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtx1 = fold_i(Fun, LOpts, FunState, NewCtxS,
        {privilegeAnnotationList@_@, maps:get(privilegeAnnotationList@,
            Value)}),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% functionArg
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{functionArg := Value} = PTree)
    when map_size(Value) == 1 ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = functionArg,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtx1 = fold_i(Fun, LOpts, FunState, NewCtxS, Value),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);
fold_i(Fun, LOpts, FunState, Ctx, #{functionArg := Value} = PTree)
    when map_size(Value) == 2 ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = functionArg,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtx1 = fold_i(Fun, LOpts, FunState, NewCtxS,
        maps:get(expression@, Value)),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% functionArgCommaList & operator & slash
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, {Rule, PTree})
    when
    (Rule == functionArgCommaList orelse Rule == operator orelse Rule == slash)
        andalso is_atom(PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% functionArgCommaList@
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, {functionArgCommaList@, PTree})
    when is_atom(PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = functionArgCommaList@,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);
fold_i(Fun, LOpts, FunState, Ctx, {functionArgCommaList@, PTree})
    when is_list(PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = functionArgCommaList@,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtx1 = list_elem_ext_rule(Fun, LOpts, FunState, NewCtxS,
        functionArgCommaList, PTree),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% functionHeading
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{functionHeading := Value} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = functionHeading,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtx1 = case maps:is_key(parameterDeclarationCommaList@, Value) of
                  true -> fold_i(Fun, LOpts, FunState, NewCtxS,
                      {parameterDeclarationCommaList@, maps:get(
                          parameterDeclarationCommaList@, Value)});
                  _ -> NewCtxS
              end,
    NewCtx2 = fold_i(Fun, LOpts, FunState, NewCtx1,
        {return, maps:get(return@, Value)}),
    NewCtxE = Fun(LOpts, FunState, NewCtx2, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% functionRef
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{functionRef := Value} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = functionRef,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtx1 = fold_i(Fun, LOpts, FunState, NewCtxS,
        {functionArgCommaList@, maps:get(functionArgCommaList@, Value)}),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% invokerRightsClause
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{invokerRightsClause := _Value} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = invokerRightsClause,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% literal
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{literal := _Value} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = literal,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% notNull
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{notNull := _Value} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = notNull,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% objectPrivilegeAnnotation
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{objectPrivilegeAnnotation := _Value} =
    PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = objectPrivilegeAnnotation,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% packageFunctionDeclaration
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunStateIn, Ctx, #{packageFunctionDeclaration := Value} =
    PTree) ->
    Rule = packageFunctionDeclaration,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtx1 = case maps:is_key(apiHiddenAnnotation@, Value) of
                  true ->
                      fold_i(Fun, LOpts, FunState, NewCtxS,
                          maps:get(apiHiddenAnnotation@, Value));
                  _ -> NewCtxS
              end,
    NewCtx2 = case maps:is_key(functionAnnotation@, Value) of
                  true ->
                      fold_i(Fun, LOpts, FunState, NewCtx1,
                          maps:get(functionAnnotation@, Value));
                  _ -> NewCtx1
              end,
    NewCtx3 = fold_i(Fun, LOpts, FunState, NewCtx2,
        maps:get(functionHeading@, Value)),
    NewCtx4 =
        case maps:is_key(packageFunctionDeclarationAttributeList@, Value) of
            true ->
                list_elem_ext_rule(Fun, LOpts, FunState, NewCtx3,
                    packageFunctionDeclarationAttributeList,
                    maps:get(packageFunctionDeclarationAttributeList@,
                        Value));
            _ -> NewCtx3
        end,
    NewCtxE = Fun(LOpts, FunState, NewCtx4, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% packageFunctionDeclarationAttribute
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx,
    #{packageFunctionDeclarationAttribute := Value} = PTree)
    when is_list(Value) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = packageFunctionDeclarationAttribute,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);
fold_i(Fun, LOpts, FunState, Ctx,
    #{packageFunctionDeclarationAttribute := Value} = PTree)
    when map_size(Value) < 3 ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = packageFunctionDeclarationAttribute,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtx1 = fold_i(Fun, LOpts, FunState, NewCtxS, Value),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% packageFunctionDeclarationAttributeList
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, {packageFunctionDeclarationAttributeList =
    Rule, Pos, PTree}) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree,
        {Rule, start, Pos}),
    NewCtx1 = fold_i(Fun, LOpts, FunState, NewCtxS, PTree),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree, {Rule, 'end', Pos}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% packageItemConditional
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{packageItemConditional := Value} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = packageItemConditional,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtx1 = case maps:is_key(expression@, Value) of
                  true ->
                      fold_i(Fun, LOpts, FunState, NewCtxS,
                          #{thenExpression@_@ => maps:get(expression@, Value)});
                  _ -> NewCtxS
              end,
    NewCtx2 = fold_i(Fun, LOpts, FunState, NewCtx1,
        maps:get(packageItem@, Value)),
    NewCtxE = Fun(LOpts, FunState, NewCtx2, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% packageItemList@_@
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{packageItemList@_@ := Value} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = packageItemList@_@,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtx1 = case maps:is_key(packageItemList@, Value) of
                  true ->
                      list_elem_ext_rule(Fun, LOpts, FunState, NewCtxS,
                          packageItemList, maps:get(packageItemList@, Value));
                  _ -> NewCtxS
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% packageItemSimple
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{packageItemSimple := Value} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = packageItemSimple,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtx1 = fold_i(Fun, LOpts, FunState, NewCtxS,
        maps:get(packageItem@, Value)),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% packageProcedureDeclaration
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunStateIn, Ctx, #{packageProcedureDeclaration := Value} =
    PTree) ->
    Rule = packageProcedureDeclaration,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtx1 = case maps:is_key(apiHiddenAnnotation@, Value) of
                  true ->
                      fold_i(Fun, LOpts, FunState, NewCtxS,
                          maps:get(apiHiddenAnnotation@, Value));
                  _ -> NewCtxS
              end,
    NewCtx2 = case maps:is_key(procedureAnnotation@, Value) of
                  true ->
                      fold_i(Fun, LOpts, FunState, NewCtx1,
                          maps:get(procedureAnnotation@, Value));
                  _ -> NewCtx1
              end,
    NewCtx3 = fold_i(Fun, LOpts, FunState, NewCtx2,
        maps:get(procedureHeading@, Value)),
    NewCtx4 = case maps:is_key(accessibleByClause@, Value) of
                  true ->
                      fold_i(Fun, LOpts, FunState, NewCtx3,
                          maps:get(accessibleByClause@, Value));
                  _ -> NewCtx3
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx4, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% parallelEnabledClause
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{parallelEnabledClause := Value} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = parallelEnabledClause,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtx1 = case maps:is_key(columnRefCommaList@, Value) of
                  true ->
                      fold_i(Fun, LOpts, FunState, NewCtxS,
                          {columnRefCommaList@,
                              maps:get(columnRefCommaList@, Value)});
                  _ -> NewCtxS
              end,
    NewCtx2 = case maps:is_key(streamingClause@, Value) of
                  true ->
                      fold_i(Fun, LOpts, FunState, NewCtx1,
                          maps:get(streamingClause@, Value));
                  _ -> NewCtx1
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx2, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% parameterDeclaration
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{parameterDeclaration := Value} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = parameterDeclaration,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtx1 = fold_i(Fun, LOpts, FunState, NewCtxS,
        maps:get(dataType@, Value)),
    NewCtx2 = case maps:is_key(default@, Value) of
                  true ->
                      fold_i(Fun, LOpts, FunState, NewCtx1,
                          maps:get(default@, Value));
                  _ -> NewCtx1
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx2, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% parameterDeclarationCommaList@
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, {parameterDeclarationCommaList@, PTree})
    when is_list(PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = parameterDeclarationCommaList@,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtx1 = list_elem_ext_rule(Fun, LOpts, FunState, NewCtxS,
        parameterDeclarationCommaList, PTree),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% parameterRef
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{parameterRef := Value} = PTree)
    when is_list(Value); is_map(Value) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = parameterRef,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% pipelinedClause
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{pipelinedClause := _Value} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = pipelinedClause,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% plsqlPackageSource
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{plsqlPackageSource := Value} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = plsqlPackageSource,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtx1 = case maps:is_key(sharingClause@, Value) of
                  true ->
                      fold_i(Fun, LOpts, FunState, NewCtxS,
                          maps:get(sharingClause@, Value));
                  _ -> NewCtxS
              end,
    NewCtx2 = case maps:is_key(plsqlPackageSourceAttributeList@, Value) of
                  true ->
                      list_elem_ext_rule(Fun, LOpts, FunState, NewCtx1,
                          plsqlPackageSourceAttributeList,
                          maps:get(plsqlPackageSourceAttributeList@, Value));
                  _ -> NewCtx1
              end,
    NewCtx3 = fold_i(Fun, LOpts, FunState, NewCtx2,
        case {maps:is_key(man_page@, Value), maps:is_key(packageItemList@,
            Value)} of
            {true, true} ->
                #{packageItemList@_@ => #{
                    asIs@=>maps:get(asIs@, Value),
                    packageItemList@=>maps:get(packageItemList@, Value),
                    man_page@=>maps:get(man_page@, Value)}};
            {true, false} ->
                #{packageItemList@_@ => #{
                    asIs@=>maps:get(asIs@, Value),
                    man_page@=>maps:get(man_page@, Value)}};
            {false, true} ->
                #{packageItemList@_@ => #{
                    asIs@=>maps:get(asIs@, Value),
                    packageItemList@=>maps:get(packageItemList@, Value)}};
            {false, false} ->
                #{packageItemList@_@ => #{asIs@=>maps:get(asIs@, Value)}}
        end),
    NewCtxE = Fun(LOpts, FunState, NewCtx3, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% plsqlPackageSourceAttribute
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{plsqlPackageSourceAttribute := Value} =
    PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = plsqlPackageSourceAttribute,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtx1 = fold_i(Fun, LOpts, FunState, NewCtxS, Value),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% plsqlUnit
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, [#{plsqlUnit := Value} = PTree]) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = plsqlUnit,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtx1 = case maps:is_key(sqlplusCommand@, Value) of
                  true ->
                      fold_i(Fun, LOpts, FunState, NewCtxS,
                          maps:get(sqlplusCommand@, Value));
                  _ -> NewCtxS
              end,
    NewCtx2 = fold_i(Fun, LOpts, FunState, NewCtx1,
        maps:get(createPackage@, Value)),
    NewCtxE = Fun(LOpts, FunState, NewCtx2, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% pragmaDefinition
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunStateIn, Ctx, #{pragmaDeclaration := Value} = PTree) ->
    Rule = pragmaDeclaration,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtx1 = case maps:is_key(pragmaParameter@, Value) of
                  true ->
                      fold_i(Fun, LOpts, FunState, NewCtxS,
                          maps:get(pragmaParameter@, Value));
                  _ -> NewCtxS
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% pragmaParameter
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{pragmaParameter := Value} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = pragmaParameter,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtx1 = case maps:is_key(restrictReferencesList@, Value) of
                  true ->
                      fold_i(Fun, LOpts, FunState, NewCtxS,
                          {restrictReferencesList@_@, maps:get(
                              restrictReferencesList@, Value)});
                  _ -> NewCtxS
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% privilegeAnnotationList@_@
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, {privilegeAnnotationList@_@, Value} =
    PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = privilegeAnnotationList@_@,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtx1 = list_elem_ext_rule(Fun, LOpts, FunState, NewCtxS,
        privilegeAnnotationList, Value),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% procedureAnnotation
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{procedureAnnotation := Value} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = procedureAnnotation,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtx1 = fold_i(Fun, LOpts, FunState, NewCtxS,
        {privilegeAnnotationList@_@, maps:get(privilegeAnnotationList@,
            Value)}),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% procedureHeading
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{procedureHeading := Value} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = procedureHeading,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtx1 = case maps:is_key(parameterDeclarationCommaList@, Value) of
                  true -> fold_i(Fun, LOpts, FunState, NewCtxS,
                      {parameterDeclarationCommaList@, maps:get(
                          parameterDeclarationCommaList@, Value)});
                  _ -> NewCtxS
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% recordTypeDefinition
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunStateIn, Ctx, #{recordTypeDefinition := Value} = PTree) ->
    Rule = recordTypeDefinition,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtx1 =
        fold_i(Fun, LOpts, FunState, NewCtxS, maps:get(recordTypeName@, Value)),
    NewCtx2 = list_elem_ext_rule(Fun, LOpts, FunState, NewCtx1,
        fieldDefinitionCommaList, maps:get(fieldDefinitionCommaList@, Value)),
    NewCtxE = Fun(LOpts, FunState, NewCtx2, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% recordTypeName
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{recordTypeName := _Value} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = recordTypeName,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% refCursorTypeDefinition
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunStateIn, Ctx, #{refCursorTypeDefinition := _Value} =
    PTree) ->
    Rule = refCursorTypeDefinition,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% restrictReferences
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{restrictReferences := _Value} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = restrictReferences,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% restrictReferencesList@_@
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, {restrictReferencesList@_@, Value} =
    PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = restrictReferencesList@_@,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtx1 = list_elem_ext_rule(Fun, LOpts, FunState, NewCtxS,
        restrictReferencesList, Value),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% resultCacheClause
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{resultCacheClause := Value} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = resultCacheClause,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtx1 = fold_i(Fun, LOpts, FunState, NewCtxS,
        {dataSourceCommaList@, maps:get(dataSourceCommaList@, Value)}),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% return
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, {return, Value} = PTree)
    when is_map(Value) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = return,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtx1 = fold_i(Fun, LOpts, FunState, NewCtxS, Value),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sharingClause
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{sharingClause := _Value} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = sharingClause,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sqlplusCommand
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{sqlplusCommand := _Value} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = sqlplusCommand,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% streamingClause
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{streamingClause := Value} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = streamingClause,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtx1 = fold_i(Fun, LOpts, FunState, NewCtxS,
        #{streamingClauseExpression@_@ => maps:get(expression@, Value)}),
    NewCtx2 = fold_i(Fun, LOpts, FunState, NewCtx1,
        {columnRefCommaList@, maps:get(columnRefCommaList@, Value)}),
    NewCtxE = Fun(LOpts, FunState, NewCtx2, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% streamingClauseExpression@_@
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{streamingClauseExpression@_@ := Value} =
    PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = streamingClauseExpression@_@,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtx1 = fold_i(Fun, LOpts, FunState, NewCtxS, Value),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% subtypeDefinition
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunStateIn, Ctx, #{subtypeDefinition := Value} = PTree) ->
    Rule = subtypeDefinition,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtx1 =
        fold_i(Fun, LOpts, FunState, NewCtxS, maps:get(subtypeName@, Value)),
    NewCtx2 = fold_i(Fun, LOpts, FunState, NewCtx1, maps:get(dataType@, Value)),
    NewCtx3 = case maps:is_key(notNull@, Value) of
                  true ->
                      fold_i(Fun, LOpts, FunState, NewCtx2,
                          maps:get(notNull@, Value));
                  _ -> NewCtx2
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx3, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% subtypeName
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{subtypeName := _Value} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = subtypeName,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% systemPrivilegeAnnotation
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{systemPrivilegeAnnotation := _Value} =
    PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = systemPrivilegeAnnotation,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% thenExpression@_@
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{thenExpression@_@ := Value} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = thenExpression@_@,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtx1 = fold_i(Fun, LOpts, FunState, NewCtxS, Value),
    NewCtxE = Fun(LOpts, FunState, NewCtx1, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% typeName
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{typeName := _Value} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = typeName,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% unaryAddOrSubtract
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{unaryAddOrSubtract := _Value} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = unaryAddOrSubtract,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% variableDeclaration
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunStateIn, Ctx, #{variableDeclaration := Value} = PTree) ->
    Rule = variableDeclaration,
    FunState = ?FOLD_INIT_STMNT(FunStateIn, Ctx, PTree, Rule),
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtx1 = fold_i(Fun, LOpts, FunState, NewCtxS, maps:get(dataType@, Value)),
    NewCtx2 = case maps:is_key(notNull@, Value) of
                  true ->
                      fold_i(Fun, LOpts, FunState, NewCtx1,
                          maps:get(notNull@, Value));
                  _ -> NewCtx1
              end,
    NewCtx3 = case maps:is_key(default@, Value) of
                  true ->
                      fold_i(Fun, LOpts, FunState, NewCtx2,
                          maps:get(default@, Value));
                  _ -> NewCtx2
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx3, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% varraySize
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{varraySize := _Value} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = varraySize,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% varrayType
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{varrayType := _Value} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = varrayType,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtxE = Fun(LOpts, FunState, NewCtxS, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% varrayTypeDef
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(Fun, LOpts, FunState, Ctx, #{varrayTypeDef := Value} = PTree) ->
    ?FOLD_INIT(FunState, Ctx, PTree),
    Rule = varrayTypeDef,
    NewCtxS = Fun(LOpts, FunState, Ctx, PTree, {Rule, start}),
    NewCtx1 =
        fold_i(Fun, LOpts, FunState, NewCtxS, maps:get(type@, Value)),
    NewCtx2 =
        fold_i(Fun, LOpts, FunState, NewCtx1, maps:get(size@, Value)),
    NewCtx3 =
        fold_i(Fun, LOpts, FunState, NewCtx2, maps:get(dataType@, Value)),
    NewCtx4 = case maps:is_key(notNull@, Value) of
                  true ->
                      fold_i(Fun, LOpts, FunState, NewCtx3,
                          maps:get(notNull@, Value));
                  _ -> NewCtx3
              end,
    NewCtxE = Fun(LOpts, FunState, NewCtx4, PTree, {Rule, 'end'}),
    ?FOLD_RESULT(NewCtxE);

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% UNSUPPORTED
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_i(_Fun, _LOpts, _FunState, _Ctx, PTree) ->
    ?FOLD_INIT(_FunState, _Ctx, PTree),
    throw({lists:append([
        "[",
        ?MODULE_STRING,
        ":",
        atom_to_list(?FUNCTION_NAME),
        "] parser subtree not supported"
    ]), PTree}).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Get current statement, clause and rule.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec get_stmnt_clause_curr(FunState :: tuple()) -> {atom(), atom(), atom()}.
get_stmnt_clause_curr(FunState) ->
    case length(FunState#fstate.stmnts) of
        0 -> {none, none, none};
        _ -> lists:last(FunState#fstate.stmnts)
    end.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper functions.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Table with external rule.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

list_elem_ext_rule(Fun, LOpts, FunState, Ctx, Rule, Elements) ->
    Length = length(Elements),
    case Length of
        0 -> Ctx;
        1 -> fold_i(Fun, LOpts, FunState, Ctx,
            {Rule, last, lists:last(Elements)});
        _ ->
            list_elem_ext_rule(Fun, LOpts, FunState, Ctx, Rule, Elements,
                Length, Length)
    end.

list_elem_ext_rule(Fun, LOpts, FunState, Ctx, Rule, [Head | Tail], Counter, Length)
    when Counter == Length ->
    NewCtxS = fold_i(Fun, LOpts, FunState, Ctx, {Rule, other, Head}),
    list_elem_ext_rule(Fun, LOpts, FunState, NewCtxS, Rule, Tail,
        Counter - 1, Length);
list_elem_ext_rule(Fun, LOpts, FunState, Ctx, Rule, [Head], 1, _Length) ->
    fold_i(Fun, LOpts, FunState, Ctx, {Rule, last, Head});
list_elem_ext_rule(Fun, LOpts, FunState, Ctx, Rule, [Head | Tail], Counter, Length) ->
    NewCtxS = fold_i(Fun, LOpts, FunState, Ctx, {Rule, other, Head}),
    list_elem_ext_rule(Fun, LOpts, FunState, NewCtxS, Rule, Tail,
        Counter - 1, Length).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Set the function state for a new statement:
% -------------------------------------------
%     collectionTypeDefinition
%     constantDeclaration
%     exceptionDeclaration
%     packageFunctionDeclaration
%     packageProcedureDeclaration
%     pragmaDeclaration
%     recordTypeDefinition
%     refCursorTypeDefinition
%     subtypeDefinition
%     variableDeclaration
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_state_stmnt(FunState, Stmnt) ->
    FunState#fstate{indent_lvl = FunState#fstate.indent_lvl + 1,
        stmnts = FunState#fstate.stmnts ++ [{Stmnt, none, none}]}.
