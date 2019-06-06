if [ -z "$1" ]
then
	echo "Usage: ./test.sh <file to test>"
	exit 1
fi

ltlcross -F "$1" 'ltl2tgba -f %f >%O' './mochi.native -f %f >%O'
