%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options

[{tests, []}].

%%
%% TESTS
%%

"
CREATE OR REPLACE PACKAGE package_1
IS
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

% ==============================================================================
