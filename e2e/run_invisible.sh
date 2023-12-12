#!/usr/bin/env bash

echo "./run_tests.sh $*" > /tmp/run_invisible.sh
chmod +x /tmp/run_invisible.sh

xvfb-run --server-args="-screen 0 1024x768x24" /tmp/run_invisible.sh
