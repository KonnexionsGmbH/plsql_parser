#!/bin/bash

exec > >(tee -i gen_tests_and_run.log)
sleep .1

# ------------------------------------------------------------------------------
#
# gen_tests_and_run.sh: PL/SQL - generate and run test data.
#
# Copyright (c) 2018-20 Konnexions GmbH.  All Rights Reserved.
#
# ------------------------------------------------------------------------------

timestamp() {
  date +"%T"
}

echo "$(timestamp) Start Test Data Generation and Run"

# Setting plsql_parser options .................................................
# "true": compacted / "false": detailed.
export GENERATE_COMPACTED="true"
export GENERATE_CT="true"
export GENERATE_EUNIT="false"
export GENERATE_PERFORMANCE="true"
export GENERATE_RELIABILITY="true"
export HEAP_SIZE="+hms 33554432"
export LOGGING="false"
export MAX_BASIC=500
test/gen_tests.sh

echo "$(timestamp) Start EUnit Tests"
SOURCEFILES_OLD=SOURCEFILES
SOURCEFILES=
rebar3 eunit
SOURCEFILES=SOURCEFILES_OLD

echo "$(timestamp) Start Common Tests"
rebar3 ct

echo "$(timestamp) Start Coverage Analysis"
rebar3 cover

echo "$(timestamp) Start Dialyzer"
rebar3 dialyzer

echo "$(timestamp) Start geas (Guess Erlang Application Scattering)"
rebar3 as test geas

echo "$(timestamp) End   Test Data Generation and Run"

exit 0
