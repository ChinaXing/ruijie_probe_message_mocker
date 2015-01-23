#!/bin/bash

[ $# -ne 4 ] && {
  echo "It missing args"
  exit 1
}

para=$1
batch=$2
mu=$3
delay=$4

erl -noinput -noshell -pa ebin -pa deps/udp_broker/ebin -pa deps/lager/ebin -pa deps/goldrush/ebin \
    -s message_mocker boot_cmdline "112.124.3.87" 12092 $para $batch $delay $mu "<<16#68f728063848:48>>" "<<16#e8b1fc3306:40>>"
#    -s message_mocker boot_cmdline "127.0.0.1" 12092 $para $batch $delay $mu "<<16#68f728063848:48>>" "<<16#e8b1fc3306:40>>"
