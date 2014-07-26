#!/bin/sh

if [ -z $ERL_LIBS ]; then
    L=`pwd`/deps
else
    L=`pwd`/deps:$ERL_LIBS
fi

dirs=`echo $L | sed 's/\:/ /g'`

for d in $dirs; do
    f=$d/setup/setup_gen
    if [ -f $f ]; then
	echo "$f"
	exit 0
    fi
done
