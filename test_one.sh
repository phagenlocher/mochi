if [ -z "$1" ]
then
	echo "Usage: ./test.sh <formula to test>"
	exit 1
fi

ltlcross -f "$1" 'ltl2tgba -f %f >%O' './mochi.native -f %f >%O'
