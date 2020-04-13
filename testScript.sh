declare -a urls=('data/creditset.csv' 'data/binary.csv' 'data/wpbc.data' 'data/wdbc.data' 'data/ionosphere.data');
declare -a headers=('T' 'T' 'F' 'F' 'F');
declare -a classPosition=(6 1 2 2 35);

echo "Enter dataset between 1-5 or enter 0 to run all at once:"
read choice

let choice=$choice-1

if [ $choice == -1 ]; then
	let start=0
	let end=4
elif [ $choice -ge 0 ] && [ $choice -le 4 ]; then
	let start=$choice
	let end=$choice
else
	echo "Wrong Choice"
	exit 1
fi


for (( i=$start; i<=$end; i++ ))
do
	echo 'Inputting url' ${urls[i]}
	Rscript classifier-comparison.R ${urls[i]} ${headers[i]} ${classPosition[i]}	
done
