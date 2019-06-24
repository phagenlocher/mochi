if [ -z "$1" ]
then
	echo "Usage: ./test_file.sh <file to test>"
	exit 1
fi

ltlcross -F "$1" 'ltl2tgba -f %f >%O' './mochi.native -b -f %f >%O'
