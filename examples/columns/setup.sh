#!/bin/sh
# Sourced before columns.t, by dune next to main.exe and by k7 cram from
# the root of the repository: the display's times follow the example's
# sleeps, and columns runs the example.
export CONSOLE_DETERMINISTIC_CLOCK=1
if [ -x main.exe ]; then
  __columns="$PWD/main.exe"
else
  __columns="$PWD/_build/default/examples/columns/main.exe"
fi
columns() { "$__columns" "$@"; }
