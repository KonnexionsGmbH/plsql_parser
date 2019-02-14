%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options

[{tests, []}].

%%
%% TESTS
%%

% ==============================================================================

"
CREATE OR REPLACE PACKAGE package_name
    SHARING = METADATA
    ACCESSIBLE BY (TYPE credit_limit_ident__5.credit_limit_ident_3)
    ACCESSIBLE BY (
        i1ident_1_,
        TYPE i#ident_8.m#oney__$tree_ident)
IS
    FUNCTION function_name_1 (i1ident_8 IN TIMESTAMP (67890123456) DEFAULT :par_65_1:par_63__)
        RETURN TIMESTAMP WITH LOCAL TIME ZONE
        PIPELINED TABLE POLYMORPHIC USING credit_limit_ident__1 /*<>Comment Line No. 189Comment Line No. 283*/
                                                               ;

    PROCEDURE i#procedure_name_1
        ACCESSIBLE BY (i#ident_200_#) /*<>Comment Line No. 306Comment Line No. 375*/
                                     ;
END;
".

