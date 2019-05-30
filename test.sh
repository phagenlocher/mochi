if [ -z "$1" ]
then
	echo "Usage: ./test.sh <number of recursive ltl depth>"
fi

while [ $? -eq 0 ]
do
formula=`./mochi.native -r $1`
ltlcross -f "$formula" 'ltl2tgba -s %s >%O' './mochi.native -f %f >%O'
done

