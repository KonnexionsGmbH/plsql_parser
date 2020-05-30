#!/bin/bash

exec > >(tee -i gen_tests.log)
sleep .1

# ------------------------------------------------------------------------------
#
# gen_tests.sh: PL/SQL - generating test data.
#
# Copyright (c) 2018-20 Konnexions GmbH.  All Rights Reserved.
#
# ------------------------------------------------------------------------------

echo "========================================================================="
echo "Start $0"
echo "-------------------------------------------------------------------------"
echo "Start Test Data Generation"
echo "-------------------------------------------------------------------------"
date +"DATE TIME : %d.%m.%Y %H:%M:%S"
echo "-------------------------------------------------------------------------"

if [ -d "_build/test/lib/plsql_parser/test/generated" ]; then
    rm -rf _build/test/lib/plsql_parser/test/generated
fi
if ls _build/test/lib/plsql_parser/test/performance_*.* 1> /dev/null 2>&1; then
    rm _build/test/lib/plsql_parser/test/performance_*.*
fi
if ls _build/test/lib/plsql_parser/test/reliability_*.* 1> /dev/null 2>&1; then
    rm _build/test/lib/plsql_parser/test/reliability_*.*
fi
if [ -d "test/generated" ]; then
    rm -rf test/generated
fi

rebar3 as test compile

# Setting plsql_parser options .................................................
if [ "$GENERATE_COMPACTED" == "" ]; then
    # true: compacted / false: detailed.
    export GENERATE_COMPACTED="true"
    export GENERATE_CT="true"
    export GENERATE_EUNIT="false"
    export GENERATE_PERFORMANCE="true"
    export GENERATE_RELIABILITY="true"
    export HEAP_SIZE="+hms 33554432"
    export LOGGING="false"
    export MAX_BASIC=500
fi

# Starting test data generator ...........................................
erl -noshell -pa _build/test/lib/plsql_parser/test $HEAP_SIZE -s plsql_parser_generator generate -s init stop

if [ -f "code_templates" ]; then
    ls -l code_templates
    rm code_templates
fi

echo "-------------------------------------------------------------------------"
date +"DATE TIME : %d.%m.%Y %H:%M:%S"
echo "-------------------------------------------------------------------------"
echo "End   $0"
echo "========================================================================="

exit 0
