#!/bin/bash
cd $(dirname $0)
exec erl \
     -pa _build/default/lib/erl_tools/ebin/ \
     -pa _build/default/lib/thermostat/ebin/ \
     -eval 'application:ensure_all_started(thermostat)'
