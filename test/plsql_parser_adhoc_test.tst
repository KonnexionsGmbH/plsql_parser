%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options

[{tests, []}].

%%
%% TESTS
%%

% ==============================================================================

"
CREATE PACKAGE package_name
AS
    my_exception                            EXCEPTION;

    FUNCTION function_name_1
        RETURN CLOB;

    PROCEDURE procedure_name_1 (
        parameter_1                             IN OUT DATE,
        parameter_2                             IN OUT DATE);
END package_name;
".

