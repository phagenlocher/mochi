if [ -z "$1" ]
then
	echo "Usage: ./test_inf.sh <number of recursive ltl depth>"
	exit 1
fi

while [ $? -eq 0 ]
do
formula=`./mochi.native -r $1`
ltlcross --stop-on-error -T 60 -f "$formula" 'ltl2tgba -f %f >%O' './mochi.native -f %f >%O'
done

