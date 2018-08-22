%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options

[{tests, []}].

%%
%% TESTS
%%

"
CREATE OR REPLACE PACKAGE session_mgmt_impl
IS
    /*<>
       Implementation package for managing session-related tasks. The most important
       functionality includes killing sessions and modifying the DBSS behaviour.
    */

    /* =========================================================================
       Public Procedure Declaration.
       ---------------------------------------------------------------------- */

    --<> role = dbss_r_session_mgmt
    PROCEDURE set_read_only_off;
    /*<>
       Sets the read-only mode to off.

       System Privileges:

          - CREATE ANY CONTEXT
    */

    PROCEDURE set_read_only_off_help;

    --<> object_privilege execute = sys.dbms_crypto
    --<> role = dbss_r_session_mgmt
    PROCEDURE set_read_only_on;
    /*<>
       Sets the read-only mode to on.

       System Privileges:

          - CREATE ANY CONTEXT
    */

    PROCEDURE set_read_only_on_help;
END session_mgmt_impl;
/
".

% ==============================================================================
