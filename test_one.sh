#!/bin/sh

if [ -z "$1" ]
then
	echo "Usage: ./test_one.sh <formula to test> <additional arguments for mochi>"
	exit 1
fi

ltlcross -f "$1" 'ltl2tgba -f %f >%O' "./mochi.native -b $2 -f %f >%O"
