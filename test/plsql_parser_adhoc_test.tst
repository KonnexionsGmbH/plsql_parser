%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options

[{tests, []}].

%%
%% TESTS
%%

"
Create Package my_package
As
--<> Object_privilege execute = my_procedure
--<> Role = my_role
Function my_procedure (
param_1 In Out Nocopy  Urowid
) return date;
End;
".

% ==============================================================================
