if [ -z "$1" ]
then
	echo "Usage: ./test_file.sh <file to test> <additional arguments for mochi>"
	exit 1
fi

ltlcross -F "$1" 'ltl2tgba -f %f >%O' "./mochi.native -b $2 -f %f >%O"
