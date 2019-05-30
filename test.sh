formula=`./mochi.native -r 3`

while [ $? -eq 0 ]
do
ltlcross -f "$formula" 'ltl2tgba -s %s >%O' './mochi.native -f %f >%O'
done

