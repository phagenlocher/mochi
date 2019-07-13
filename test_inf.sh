if [ -z "$1" ]
then
	echo "Usage: ./test_inf.sh <number of recursive ltl depth> <additional arguments for mochi>"
	exit 1
fi

while [ $? -eq 0 ]
do
formula=`./mochi.native -r $1`
ltlcross --stop-on-error -T 120 -f "$formula" 'ltl2tgba -f %f >%O' "./mochi.native -b $2 -f %f >%O"
done

